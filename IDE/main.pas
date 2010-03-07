unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, XPMan, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, Menus, ExtCtrls, ComCtrls, uFrmAbout, uFrmSearch, SynEditTypes,
  SynEditRegExSearch,

  uFormCodeEditor, uFormCompileMessages, uScriptProject, SynHighlighterSE2,
  uPackageInfo,

  uSE2Compiler, uSE2PEData, uSE2Reader, uSE2RunTime, uSE2UnitCacheMngr, uSE2OpCode, uSE2DebugData,
  SynEditMiscClasses, SynEditSearch, SynEdit, Buttons;

type
  TMainForm = class(TForm)
    mainClient: TPanel;
    MainMenu: TMainMenu;
    MainMenu_File: TMenuItem;
    File_NewProject: TMenuItem;
    File_Open: TMenuItem;
    File_Save: TMenuItem;
    File_Sep_2: TMenuItem;
    File_Close: TMenuItem;
    MainMenu_Edit: TMenuItem;
    Edit_UnDo: TMenuItem;
    Edit_ReDo: TMenuItem;
    Edit_Sep_1: TMenuItem;
    Edit_Cut: TMenuItem;
    Edit_Copy: TMenuItem;
    Edit_Paste: TMenuItem;
    Edit_Sep_2: TMenuItem;
    Edit_Delete: TMenuItem;
    Edit_SelectAll: TMenuItem;
    MainMenu_Search: TMenuItem;
    Search_Search: TMenuItem;
    Search_SearchNext: TMenuItem;
    Search_Sep_1: TMenuItem;
    Search_GoToLine: TMenuItem;
    MainMenu_View: TMenuItem;
    View_ShowMessages: TMenuItem;
    View_SearchResult: TMenuItem;
    View_DebugMessages: TMenuItem;
    MainMenu_Project: TMenuItem;
    Project_Compile: TMenuItem;
    MainMenu_Run: TMenuItem;
    Run_Run: TMenuItem;
    Run_Stop: TMenuItem;
    ImageList1: TImageList;
    hSplitter1: TSplitter;
    mainLeft: TPageControl;
    tabProject: TTabSheet;
    tvProjectTree: TTreeView;
    ImgListProject: TImageList;
    File_Sep_3: TMenuItem;
    projectPopup: TPopupMenu;
    Project_AddNewUnit: TMenuItem;
    Project_AddNewFolder: TMenuItem;
    StatusBar1: TStatusBar;
    File_CloseProject: TMenuItem;
    Run_Step: TMenuItem;
    panelDebugging: TPanel;
    stepDebugStack: TListView;
    stepDebugInstructions: TListBox;
    Project_SaveToFile: TMenuItem;
    Splitter1: TSplitter;
    MainMenu_Help: TMenuItem;
    Help_About: TMenuItem;
    Project_Sep1: TMenuItem;
    Project_RemoveItem: TMenuItem;
    panelLeft: TPanel;
    SynEditRegexSearch: TSynEditRegexSearch;
    SynEditSearch: TSynEditSearch;
    Project_Sep2: TMenuItem;
    Project_GenUnit: TMenuItem;
    GenUnit_Application: TMenuItem;
    GenUnit_Package: TMenuItem;
    Project_Make: TMenuItem;
    tabProjects: TTabSheet;
    listProjects: TListView;
    Splitter2: TSplitter;
    stepCallStack: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure File_CloseClick(Sender: TObject);
    procedure View_DebugMessagesClick(Sender: TObject);
    procedure File_NewProjectClick(Sender: TObject);
    procedure Project_PopupClick(Sender: TObject);
    procedure tvProjectTreeDblClick(Sender: TObject);
    procedure tvProjectTreeCompare(Sender: TObject; Node1,
      Node2: TTreeNode; Data: Integer; var Compare: Integer);
    procedure Project_CompileClick(Sender: TObject);
    procedure Run_RunClick(Sender: TObject);
    procedure Run_StopClick(Sender: TObject);
    procedure File_SaveClick(Sender: TObject);
    procedure File_OpenClick(Sender: TObject);
    procedure File_CloseProjectClick(Sender: TObject);
    procedure Run_StepClick(Sender: TObject);
    procedure stepDebugInstructionsDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Project_SaveToFileClick(Sender: TObject);
    procedure Help_AboutClick(Sender: TObject);
    procedure MainMenu_EditClick(Sender: TObject);
    procedure Edit_SelectAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Search_SearchClick(Sender: TObject);
    procedure Search_SearchNextClick(Sender: TObject);
    procedure Search_GoToLineClick(Sender: TObject);
    procedure MainMenu_SearchClick(Sender: TObject);
    procedure MainMenu_FileClick(Sender: TObject);
    procedure projectPopupPopup(Sender: TObject);
    procedure packageTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure GenUnit_PackageClick(Sender: TObject);
    procedure listProjectsDblClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FUnitCache      : TSE2UnitCacheMngr;
    FCodeEditorMngr : TScriptEditorMngr;
    FProjectFile    : TScriptProject;

    FRunTime        : TSE2RunTime;

    procedure ProjectsLoad;
    procedure ProjectsSave;
    procedure ProjectsAdd(const FileName: string);
  protected
    FDoAbort        : boolean;
    FCanContinue    : boolean;
    FLastIDLETime   : cardinal;

    function  SaveProjectTo: boolean;
    function  DoSaveUnitTab(Tab: TCodeEditorPanel): boolean;

    procedure EditorRequireUnit(Sender: TObject; UnitData: TObject);
    procedure EditorRequireSave(Sender: TObject; CodeTab: TCodeEditorPanel; var CanClose: boolean);
    procedure EditorGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);
    procedure EditorGetCustomUnits(Sender: TObject; const Target: TStrings);
    procedure CompilerGetReader(Sender: TObject; const Name: string; const Readers: TList);
    procedure RunTimeBeforeCall(Sender: TObject);
    procedure RunTimeBeforeStepCall(Sender: TObject);
                         
    procedure ClearOpCodes;
    procedure ShowOpCodes(Data: TSE2PE; DebugPanel: boolean);
    procedure ShowCurrentStack(Data: TSE2RunTime);

  protected
    // Search
    FPackageInspector      : TIDEPackageInspector;
    FSearchFromCaret       : boolean;
    
    function  DoSearchReplaceText(AReplace, ABackwards: boolean): boolean;
    procedure ShowSearchReplaceDialog(AReplace: boolean);
  public
    { Public-Deklarationen }
    procedure Project_NeedCloseUnit(Sender: TObject; AUnit: TScriptUnit);
    procedure Project_AddUnit(Parent: TTreeNode);
    procedure Project_AddFolder(Parent: TTreeNode);
    procedure Project_RemoveEntry(Node: TTreeNode);
    function  Project_GetPath(aNode: TTreeNode): string;

    procedure CloseProject;
    procedure DoNewProject(const Title: string = '');
    procedure DoOpenProject(const FileName: string);
    procedure FillProjectTree();
    function  CheckUnsavedDocuments: boolean;

    function  DoCompile: TSE2PE;

    procedure UpdateCaption(const CustomText: string = '');
    procedure DoRunScript(Data: TSE2PE; Stepping: boolean);
    procedure DoOpenUnit(aUnit: TScriptUnit);
  end;

var
  MainForm: TMainForm;

implementation

uses
  ConsoleForm, uSE2RunType, Math, uSE2RunAccess, uPackageLoader,
  uSE2Packages, uSE2UnitManager, uSE2ScriptImporter,

  uSE2IncHelpers;

const
  SScriptExecuting      = 'The script is still executing. Do you want to abort the execution?';
  STooManyUnsaved       = 'There are more than one unsaved documents opened!'#13#10+
                          'If you continue now, no changes will be saved!'#13#10+
                          'Do you want to continue?'#13#10'Modified files:'#13#10;
  SProjectFiles         = 'Project files';
  SSelectTargetFolder   = 'Please select the project target folder';
  SSavePromt            = 'Do you want to save the changes for %s?';
  SExecutedIn           = 'Executed in %s sec';
  SExecutingAction      = '- [running]';
  SCompiledInSec        = 'Compiled successfully in %s sec';
  SCompiledLines        = 'Compiled lines: %d lines';
  SCompiledSize         = 'Compiled executable size: %s KB';
  SCompileInfoStr       = 'Info';
  SNewFolderName        = 'Folder';
  SNewFolderTitle       = 'New folder';
  SNewFolderDescr       = 'Enter the name of the new folder';
  SNewUnitName          = 'Unit';
  SNewUnitTitle         = 'New unit';
  SNewUnitDescr         = 'Enter the name of the new unit';
  SUnitAlreadyExists    = 'The unit name "%s" already exists in the selected folder';
  SNewProjectName       = 'Project1';
  SNewProjectTitle      = 'New Project';
  SNewProjectDescr      = 'Please enter the name of the project';
  SProjectNameTooShort  = 'The project name must have at least one character';
  SProjectNameNoNumber  = 'The project name must not start with a number';
  SProjectNameInvalid   = 'The character at position %d is not valid';     
  SFolderAlreadyExists  = 'The folder "%s" already exists in the selected folder';
  STextNotFound         = 'Search complete'; 
  SLineNumberTitle      = 'Enter line number';
  SLineNumberCaption    = 'Enter the line number, you want to jump to'; 
  SInvalidNumber        = 'You did not enter a valid number!';
  SNumberNotInRange     = 'The line number must be between 1 and %d!';

{$R *.dfm}

{ ************************************************
  *             Basic Form Actions               *
  ************************************************ }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FUnitCache := TSE2UnitCacheMngr.Create;
  LoadPackages(ExtractFilePath(Application.ExeName) + 'packages\', '.dll');


  FPackageInspector := TIDEPackageInspector.Create(mainClient);
  FPackageInspector.Parent := mainClient;
  FPackageInspector.SizeImageList := ImageList1;
  FPackageInspector.ToggleUpIndex := 20;
  FPackageInspector.ToggleDownIndex := 19;
  FPackageInspector.CreateComponents(ImgListProject);
  TCodeEditor.SetupColors(FPackageInspector.Highlighter, FPackageInspector.Editor);
  FPackageInspector.FillTree;

  FCodeEditorMngr := TScriptEditorMngr.Create(Self);
  FCodeEditorMngr.Parent := mainClient;
  FCodeEditorMngr.Align  := alClient;
  FCodeEditorMngr.CreateEditor;
  FCodeEditorMngr.Editor.OnGetUnit     := CompilerGetReader;
  FCodeEditorMngr.Editor.OnGetUnitMngr := EditorGetUnitMngr;
  FCodeEditorMngr.Editor.OnGetCustomUnits := EditorGetCustomUnits;
  FCodeEditorMngr.Editor.OnRequireSave := EditorRequireSave;
  FCodeEditorMngr.Editor.OnRequireUnit := EditorRequireUnit;

  FProjectFile := TScriptProject.Create;
  FProjectFile.OnCloseUnit := Project_NeedCloseUnit;
  FCodeEditorMngr.Messages.AddPanel(panelDebugging, 'Step Debugging');
  FCodeEditorMngr.Messages.AddPanel(FPackageInspector, 'Registered Packages');

  DoNewProject(SNewProjectName);
  ProjectsLoad;
  mainLeft.ActivePageIndex := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPackageInspector.Free;
  FUnitCache.Free;
  FProjectFile.Free;
  FCodeEditorMngr.Free;
  ProjectsSave;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Run_Stop.Enabled then
  begin
    CanClose := True;
    case MessageDlg(SScriptExecuting, mtWarning, [mbYes, mbNo], 0) of
    ID_YES : Run_StopClick(nil);
    ID_NO  : begin
               CanClose := False;
               exit;
             end;
    end;
  end;

  CanClose := CheckUnsavedDocuments;
end;

{ ************************************************
  *               Project Methods                *
  ************************************************ }

procedure TMainForm.UpdateCaption(const CustomText: string = '');
var s: string;
begin
  s := 'ScriptEngine II';
  if FProjectFile.ProjectFile.ProjectName <> '' then
     s := FProjectFile.ProjectFile.ProjectName + ' - ' + s;

  if CustomText <> '' then
     s := s + ' ' + CustomText;

  Caption := s;
  Application.Title := s;
end;

procedure TMainForm.CloseProject;
begin
  if Run_Stop.Enabled then
     raise EAbort.Create('');
  FUnitCache.Clear;
  FProjectFile.Close;

  FCodeEditorMngr.Editor.Clear;
  FCodeEditorMngr.Messages.Messages.Clear;
  FCodeEditorMngr.Messages.Search.Clear;
  FCodeEditorMngr.Messages.RunTime.Clear;
  ClearOpCodes;

  tvProjectTree.Items.Clear;
  UpdateCaption;

  MainMenu_Project.Enabled := False;
  MainMenu_Run.Enabled := False;
end;

procedure TMainForm.DoNewProject(const Title: string = '');
var s: string;
    i: integer;
    b: boolean;
begin

  if Title <> '' then
    s := Title
  else
    s := SNewProjectName;

  while (Title = '') do
  begin
    if not InputQuery(SNewProjectTitle, SNewProjectDescr, s) then
       exit;

    if (length(s) < 1) then
       MessageDlg(SProjectNameTooShort, mtError, [mbOk], 0)
    else
    if s[1] in ['0'..'9'] then
       MessageDlg(SProjectNameNoNumber, mtError, [mbOK], 0)
    else
    begin
      b := True;
      for i:=1 to length(s) do
      begin
        if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9']) then
        begin
          b := False;
          MessageDlg(Format(SProjectNameInvalid, [i]), mtError, [mbOK], 0);
          break;
        end;
      end;
      if b then
         break;
    end;
  end;
  CloseProject;      
  FProjectFile.NewProject(s);
  DoOpenUnit(FProjectFile.AProgram);
  FillProjectTree();
  UpdateCaption;

  
  MainMenu_Project.Enabled := True;
  MainMenu_Run.Enabled := True;
end;

procedure TMainForm.DoOpenUnit(aUnit: TScriptUnit);
var aCaption : string;
begin
  if aUnit = nil then
     exit;

  aCaption := aUnit.UnitName;
  if aCaption = '' then
     aCaption := FProjectFile.ProjectFile.ProjectName;
  with FCodeEditorMngr.Editor.Open(aCaption) do
  begin
    Data := aUnit;
    Editor.Source := aUnit.Source;
  end;
end;

procedure TMainForm.FillProjectTree;

  function FindNode(SearchRoot: TTreeNode; Names: TStringList): TTreeNode; overload;
  var i    : integer;
      Node : TTreeNode;
  begin
    result := nil;
    if Names.Count = 0 then
       exit;

    Node := nil;
    for i:=0 to SearchRoot.Count-1 do
    begin
      if SameText(SearchRoot[i].Text, Names[0]) then
      begin
        Node := SearchRoot[i];
        break;
      end;
    end;
    if Node = nil then
    begin
      Node := tvProjectTree.Items.AddChild(SearchRoot, Names[0]);
      Node.ImageIndex := 0;
      Node.SelectedIndex := 0;
    end;
    Names.Delete(0);

    if Names.Count = 0 then
       result := Node
    else
       result := FindNode(Node, Names);
  end;

  function FindNode(Path: string): TTreeNode; overload;
  var sl: TStringList;
  begin
    result := tvProjectTree.Items[0];
    sl := TStringList.Create;
    try
      sl.Text := StringReplace(Path, '\', #13#10, [rfReplaceAll]);
      if sl.Count > 0 then
        if sl[0] = '' then
          sl.Delete(0);

      if sl.Count > 0 then
         result := FindNode(result, sl);
    finally
      sl.Free;
    end;
  end;

var i    : integer;
    Root : TTreeNode;
begin
  tvProjectTree.Items.BeginUpdate;
  try
    tvProjectTree.Items.Clear;
    with tvProjectTree.Items.AddChild(nil, FProjectFile.ProjectFile.ProjectName) do
    begin
      Data           := FProjectFile.AProgram;
      ImageIndex     := 2;
      SelectedIndex  := 2;
    end;

    for i:=0 to FProjectFile.ProjectFile.UnitList.Count-1 do
    begin
      Root := FindNode(ExtractFileDir(FProjectFile.ProjectFile.UnitPath[i]));
      if FProjectFile.ProjectFile.UnitList[i] <> '' then
      begin
        Root := tvProjectTree.Items.AddChild(Root, ExtractFileName(FProjectFile.ProjectFile.UnitPath[i]));
        Root.Data := nil;
        Root.ImageIndex := 4;
        Root.SelectedIndex := 4;
      end;
    end;

    tvProjectTree.Items[0].Expand(True);
    tvProjectTree.CustomSort(nil, 0);
  finally
    tvProjectTree.Items.EndUpdate;
  end;
end;

function TMainForm.CheckUnsavedDocuments: boolean;
var i             : integer;
    numModified   : integer;
    ModifiedFiles : string;
    tmpValue      : boolean;
begin
  result := True;
  
  numModified   := 0;
  ModifiedFiles := '';
  for i:=0 to FCodeEditorMngr.Editor.Count-1 do
    if FCodeEditorMngr.Editor[i].Modified then
    begin
      inc(numModified);
      ModifiedFiles := ModifiedFiles + #13#10#9 + FCodeEditorMngr.Editor[i].Caption;
    end;

  if numModified = 0 then
  begin
    result := True;
    exit;
  end;

  if numModified = 1 then
  begin
    result := True;

    for i:=0 to FCodeEditorMngr.Editor.Count-1 do
      if FCodeEditorMngr.Editor[i].Modified then
      begin
        tmpValue := True;
        EditorRequireSave(nil, FCodeEditorMngr.Editor[i], tmpValue);
        if not tmpValue then
           result := False;
        exit;
      end;
  end else
  begin
    case MessageDlg(STooManyUnsaved + ModifiedFiles, mtWarning, [mbYes, mbNo], 0) of
    ID_YES : result := True;
    ID_NO  : result := False;
    end;
  end;
end;

{ ************************************************
  *              Project Methods                 *
  ************************************************ }

function TMainForm.Project_GetPath(aNode: TTreeNode): string;
begin
  result := '';
  if aNode.ImageIndex <> 0 then
     aNode := aNode.Parent;
  while aNode <> nil do
  begin
    if aNode.ImageIndex = 0 then
       result := aNode.Text + '\' + result;
    aNode := aNode.Parent;
  end;
end;

procedure TMainForm.Project_RemoveEntry(Node: TTreeNode);
var isFolder : boolean;
    FileName : string;
begin
  if Node = tvProjectTree.Items[0] then
     exit;

  isFolder := Node.ImageIndex = 0;
  FileName := Project_GetPath(Node);

  if not isFolder then
     FileName := FileName + Node.Text;

  if isFolder then
     FProjectFile.RemoveFolder(FileName)
  else
    FProjectFile.RemoveFile(FileName);

  Node.Free;
end;

procedure TMainForm.Project_AddUnit(Parent: TTreeNode);
var aFolder : string;
    prnt    : TTreeNode;
    s       : string;
    Name    : string;
    NewNode : TTreeNode;
    i       : integer;
begin
  aFolder := '';
  prnt := Parent;
  if prnt.ImageIndex <> 0 then
     prnt := prnt.Parent;
  while prnt <> nil do
  begin
    if prnt.ImageIndex = 0 then
       aFolder := prnt.Text + '\' + aFolder;
    prnt := prnt.Parent;
  end;

  Name := SNewUnitName;
  i := 1;
  while FProjectFile.ProjectFile.UnitPath.IndexOf(aFolder + Name + IntToStr(i) + '.script') >= 0 do
    i := i + 1;
  Name := Name + IntToStr(i);

  if InputQuery(SNewUnitTitle, SNewUnitDescr, Name) then
  begin
    s := aFolder + Name + '.script';
    if FProjectFile.ProjectFile.UnitPath.IndexOf(s) = -1 then
    begin
      FProjectFile.ProjectFile.UnitPath.Add(s);
      FProjectFile.ProjectFile.UnitList.Add(Name);

      NewNode := tvProjectTree.Items.AddChild(Parent, Name + '.script');
      NewNode.Data := nil;
      NewNode.ImageIndex := 4;
      NewNode.SelectedIndex := 4;
      tvProjectTree.CustomSort(nil, 0);
      Parent.Expand(False);
    end else
      MessageDlg(Format(SUnitAlreadyExists, [Name]), mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.Project_AddFolder(Parent: TTreeNode);
var aFolder : string;
    prnt    : TTreeNode;
    s       : string;
    Name    : string;       
    NewNode : TTreeNode;
    i       : integer;
begin
  aFolder := '';
  prnt := Parent;
  if prnt.ImageIndex <> 0 then
     prnt := prnt.Parent;
  while prnt <> nil do
  begin
    if prnt.ImageIndex = 0 then
       aFolder := prnt.Text + '\' + aFolder;
    prnt := prnt.Parent;
  end;

  Name := SNewFolderName;
  i := 1;
  while FProjectFile.ProjectFile.UnitPath.IndexOf(aFolder + Name + IntToStr(i) + '\') >= 0 do
    i := i + 1;
  Name := Name + IntToStr(i);

  if InputQuery(SNewFolderTitle, SNewFolderDescr, Name) then
  begin
    s := aFolder + Name + '\';
    if FProjectFile.ProjectFile.UnitPath.IndexOf(s) = -1 then
    begin
      FProjectFile.ProjectFile.UnitPath.Add(s);
      FProjectFile.ProjectFile.UnitList.Add('');

      NewNode := tvProjectTree.Items.AddChild(Parent, Name);
      NewNode.Data := nil;
      NewNode.ImageIndex := 0;
      NewNode.SelectedIndex := 0;
      tvProjectTree.CustomSort(nil, 0);
      Parent.Expand(False);
    end else                    
      MessageDlg(Format(SFolderAlreadyExists, [Name]), mtError, [mbOK], 0);
  end;
end;

{ ************************************************
  *              Internal Events                 *
  ************************************************ }

procedure TMainForm.Project_NeedCloseUnit(Sender: TObject; AUnit: TScriptUnit);
begin
  FUnitCache.ClearCache(AUnit.UnitName);
  FCodeEditorMngr.Editor.Close(AUnit);
end;

{ ************************************************
  *             Project Tree Events              *
  ************************************************ }

procedure TMainForm.Project_PopupClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
     exit;

  case (Sender as TMenuItem).Tag of
  1  : begin
         if tvProjectTree.Selected <> nil then
            Project_AddUnit(tvProjectTree.Selected);
       end;
  2  : begin
         if tvProjectTree.Selected <> nil then
            Project_AddFolder(tvProjectTree.Selected);
       end;
  10 : begin
         if tvProjectTree.Selected <> nil then
            Project_RemoveEntry(tvProjectTree.Selected);
       end;
  end;
end;

procedure TMainForm.tvProjectTreeDblClick(Sender: TObject);
var aUnit : TScriptUnit;
    Editor: TCodeEditorPanel;
begin
  if tvProjectTree.Selected = nil then
     exit;

  if (tvProjectTree.Selected.ImageIndex = 4) or (tvProjectTree.Selected.ImageIndex = 2) then
  begin
    if tvProjectTree.Selected.Data = nil then
    begin
      aUnit := FProjectFile.Files[Project_GetPath(tvProjectTree.Selected) + tvProjectTree.Selected.Text];
      tvProjectTree.Selected.Data := aUnit;
    end else
    begin
      aUnit := tvProjectTree.Selected.Data;
    end;
    if aUnit <> nil then
    begin
      Editor := FCodeEditorMngr.Editor.GetByData(aUnit);
      if Editor <> nil then
         FCodeEditorMngr.Editor.Selected := Editor
      else
         DoOpenUnit(aUnit);
    end;
  end;
end;

procedure TMainForm.tvProjectTreeCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  if Node1.ImageIndex = 0 then
  begin
    if Node2.ImageIndex <> 0 then
       Compare := -1
    else
       Compare := CompareStr(Node1.Text, Node2.Text);
  end else
  begin
    if Node2.ImageIndex = 0 then
       Compare := 1
    else
       Compare := CompareStr(Node1.Text, Node2.Text);
  end;
end;

{ ************************************************
  *                 Menu Events                  *
  ************************************************ }

procedure TMainForm.File_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.View_DebugMessagesClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
     exit;

  case TMenuItem(Sender).Tag of
  1 : FCodeEditorMngr.Messages.ViewType := mvMessages;
  2 : FCodeEditorMngr.Messages.ViewType := mvSearch;
  3 : FCodeEditorMngr.Messages.ViewType := mvRunMessage;
  end;
end;

procedure TMainForm.File_NewProjectClick(Sender: TObject);
begin
  if CheckUnsavedDocuments then
     DoNewProject;
end;       

procedure TMainForm.File_SaveClick(Sender: TObject);
begin
  DoSaveUnitTab(FCodeEditorMngr.Editor.Selected);
end;

procedure TMainForm.projectPopupPopup(Sender: TObject);
begin
  Project_AddNewUnit.Enabled := tvProjectTree.Selected <> nil;
  Project_AddNewFolder.Enabled := Project_AddNewUnit.Enabled;
  Project_RemoveItem.Enabled := Project_AddNewUnit.Enabled;
  if Project_RemoveItem.Enabled then
     Project_RemoveItem.Enabled := tvProjectTree.Selected <> tvProjectTree.Items[0];
end;

procedure TMainForm.MainMenu_EditClick(Sender: TObject);
var i      : integer;
    Editor : TCodeEditor;
begin
  if FCodeEditorMngr.Editor.Selected = nil then
  begin
    for i:=MainMenu_Edit.Count-1 downto 0 do
      if MainMenu_Edit.Items[i].Caption <> '-' then
         MainMenu_Edit.Items[i].Enabled := False;
    exit;
  end;

  Editor := FCodeEditorMngr.Editor.Selected.Editor;

  Edit_UnDo.Enabled := Editor.Editor.CanUndo;
  Edit_ReDo.Enabled := Editor.Editor.CanRedo;
  Edit_Cut.Enabled  := Editor.Editor.SelLength > 0;
  Edit_Copy.Enabled := Editor.Editor.SelLength > 0;
  Edit_Paste.Enabled := Editor.Editor.CanPaste;
  Edit_Delete.Enabled := Editor.Editor.SelLength > 0;
  Edit_SelectAll.Enabled := True;
end;         

procedure TMainForm.MainMenu_SearchClick(Sender: TObject);
begin
  Search_Search.Enabled := FCodeEditorMngr.Editor.Selected <> nil;
  Search_SearchNext.Enabled := Search_Search.Enabled;
  Search_GoToLine.Enabled   := Search_Search.Enabled;
end;

procedure TMainForm.MainMenu_FileClick(Sender: TObject);
begin
  File_Save.Enabled := FProjectFile.AProgram <> nil;
  File_CloseProject.Enabled := File_Save.Enabled;
end;

procedure TMainForm.Edit_SelectAllClick(Sender: TObject);
var Editor : TCodeEditor;
begin
  if FCodeEditorMngr.Editor.Selected = nil then
     exit;
  if not (Sender is TMenuItem) then
     exit;
                        
  Editor := FCodeEditorMngr.Editor.Selected.Editor;
  case TMenuItem(Sender).Tag of
  1 : Editor.Editor.Undo;
  2 : Editor.Editor.Redo;
  3 : Editor.Editor.CopyToClipboard;
  4 : Editor.Editor.CutToClipboard;
  5 : Editor.Editor.PasteFromClipboard;
  6 : Editor.Editor.ClearSelection;
  7 : Editor.Editor.SelectAll;
  end;
end;       

procedure TMainForm.Help_AboutClick(Sender: TObject);
begin
  with TfrmAbout.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{ ************************************************
  *             Project Menu Events              *
  ************************************************ }

procedure TMainForm.Project_CompileClick(Sender: TObject);
var PEData    : TSE2PE;
    c, t1, t2 : int64;
begin
  if Sender is TMenuItem then
    if TMenuItem(Sender).Tag = 2 then
      FUnitCache.Clear;

  QueryPerformanceFrequency(c);
  QueryPerformanceCounter(t1);
  PEData := DoCompile;
  QueryPerformanceCounter(t2);

  ShowOpCodes(PEData, False);

  PEData.Free;
end;

procedure TMainForm.Run_RunClick(Sender: TObject);
var PE: TSE2PE;
begin
  if Run_Stop.Enabled then
  begin
    FCanContinue := True;
    FRunTime.OnBeforeOperation := RunTimeBeforeCall;
    exit;  
  end;

  PE := DoCompile;
  if PE <> nil then
  begin
    ShowOpCodes(PE, False);
    DoRunScript(PE, False);
  end;
end;

procedure TMainForm.Run_StepClick(Sender: TObject);
var PE: TSE2PE;
begin
  if Run_Stop.Enabled then
  begin
    FCanContinue := True;
    exit;
  end;
  PE := DoCompile;
  if PE <> nil then
  begin
    FCanContinue := False;
    panelDebugging.Top := 0;
    panelDebugging.Visible := True;
    try
      ShowOpCodes(PE, True);
      DoRunScript(PE, True);
    finally
      ClearOpCodes;
    end;
  end;
end;   

procedure TMainForm.Run_StopClick(Sender: TObject);
begin
  FDoAbort := True;
  ConsoleForm.CloseConsole
end;

{ ************************************************
  *              Script Engine Events            *
  ************************************************ }

procedure TMainForm.CompilerGetReader(Sender: TObject; const Name: string;
  const Readers: TList);
var i      : integer;
    Item   : TScriptUnit;
    Editor : TCodeEditorPanel;
begin
  for i:=0 to FProjectFile.ProjectFile.UnitList.Count-1 do
  begin
    if SameText(FProjectFile.ProjectFile.UnitList[i], Name) then
    begin
      Item   := FProjectFile.Files[FProjectFile.ProjectFile.UnitPath[i]];
      Editor := FCodeEditorMngr.Editor.GetByData(Item);

      if Editor <> nil then
         Readers.Add(TSE2StringReader.Create(Editor.Editor.Source, Editor.Data))
      else
      if not Item.Compiled then
         Readers.Add(TSE2StringReader.Create(Item.Source, Item));

    end;
  end;
end;

procedure TMainForm.RunTimeBeforeStepCall(Sender: TObject);
var run: TSE2RunTime;

  function SearchCodePos(const currentPos: integer): integer;
  begin
    try
      for result:=currentPos to stepDebugInstructions.Items.Count-1 do
        if stepDebugInstructions.Items.Objects[result] = TObject(currentPos + 1) then
          exit;
    except
    end;
    result := -1;
  end;

  function ListBoxVisibleItems: integer;
  begin
    result := stepDebugInstructions.ItemAtPos(Point(5, stepDebugInstructions.ClientHeight - 2), False) -
              stepDebugInstructions.ItemAtPos(Point(5, 2), False);
  end;

var newSelIndex : integer;
begin
  run := TSE2RunTime(Sender);
  if FDoAbort then
     run.Abort;

  FCanContinue := False;
  newSelIndex := SearchCodePos(run.CodePos);
  if newSelIndex > -1 then
  begin

     stepDebugInstructions.ItemIndex := Max(0, newSelIndex - (ListBoxVisibleItems div 2));
     stepDebugInstructions.ItemIndex := Min(stepDebugInstructions.Count-1, newSelIndex + (ListBoxVisibleItems div 2));
     stepDebugInstructions.ItemIndex := newSelIndex;
  end;

  FCodeEditorMngr.Messages.SetActiveIndex(3);
  ShowCurrentStack(run);
  while (not FCanContinue) and (not FDoAbort) do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  
  if FDoAbort then
     run.Abort;
end;

procedure TMainForm.RunTimeBeforeCall(Sender: TObject);
begin
  if FDoAbort then
     TSE2RunTime(Sender).Abort;

  if GetTickCount - FLastIDLETime > 33 then
  begin
    FLastIDLETime := GetTickCount;
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.EditorGetUnitMngr(Sender: TObject;
  var UnitMngr: TSE2UnitCacheMngr);
begin
  UnitMngr := Self.FUnitCache;
end;

procedure TMainForm.EditorGetCustomUnits(Sender: TObject;
  const Target: TStrings);
var i: integer;
begin
  for i:=0 to FProjectFile.ProjectFile.UnitList.Count-1 do
    if FProjectFile.ProjectFile.UnitList[i] <> '' then
      Target.Add(FProjectFile.ProjectFile.UnitList[i]);
end;

{ ************************************************
  *               .....  .... ....               *
  ************************************************ }

function TMainForm.DoCompile: TSE2PE;
var Compiler : TSE2Compiler;
    Editor   : TCodeEditorPanel;
    i        : integer;
    c, t1, t2: int64;
begin
  for i:=0 to FCodeEditorMngr.Editor.Count-1 do
    FCodeEditorMngr.Editor.Items[i].Editor.ErrorLine := -1;

  Screen.Cursor := crHourGlass;

  QueryPerformanceFrequency(c);
  Compiler := TSE2Compiler.Create;
  try
    Compiler.UnitCache := FUnitCache;
    FCodeEditorMngr.Messages.Messages.Clear;
    FCodeEditorMngr.Messages.RegisterCompiler(Compiler);
    Compiler.OnGetFile := CompilerGetReader;
    
    Editor := FCodeEditorMngr.Editor.GetByData(FProjectFile.AProgram);

    QueryPerformanceCounter(t1);
    if Editor <> nil then
       result := Compiler.Compile(TSE2StringReader.Create(Editor.Editor.Source, FProjectFile.AProgram))
    else
       result := Compiler.Compile(TSE2StringReader.Create(FProjectFile.AProgram.Source, FProjectFile.AProgram));
    QueryPerformanceCounter(t2);

    if result <> nil then
    begin
      with FCodeEditorMngr.Messages.Messages.Messages.Items.Add do
      begin
        Caption := SCompileInfoStr;
        SubItems.Add('');
        SubItems.Add(Format(SCompiledInSec, [FloatToStrF((t2 - t1) / c, ffNumber, 8, 2)]));
      end;

      with FCodeEditorMngr.Messages.Messages.Messages.Items.Add do
      begin
        Caption := SCompileInfoStr;
        SubItems.Add('');
        SubItems.Add(Format(SCompiledLines, [Compiler.CompiledLines]));
      end;

      with FCodeEditorMngr.Messages.Messages.Messages.Items.Add do
      begin
        Caption := SCompileInfoStr;
        SubItems.Add('');
        SubItems.Add(Format(SCompiledSize, [FloatToStrF(result.FinalStreamSize / 1024, ffNumber, 8, 1)]));
      end;
    end;
  finally
    Screen.Cursor := crDefault;

    Compiler.Free;
  end;
end;          

procedure TMainForm.ClearOpCodes;
begin
  stepDebugInstructions.Clear;
  stepDebugStack.Clear;
  stepCallStack.Clear;
end;

procedure TMainForm.ShowOpCodes(Data: TSE2PE; DebugPanel: boolean);
var i    : integer;
    meta : TSE2MetaEntry;
    minIndex : integer;

  function LeadingZero(i: integer; min: integer): string;
  begin
    result := IntToStr(i);
    while length(result) < min do
      result := '0' + result;
  end;

  function GetMinIndex(i: integer): integer;
  begin
    result := length(IntToStr(i));
  end;

begin
  if DebugPanel then
  begin
    stepDebugInstructions.Items.BeginUpdate;
    try
      stepDebugInstructions.Clear;
      if Data <> nil then
      begin
        minIndex := GetMinIndex(Data.OpCodes.Count);
        for i:=0 to Data.OpCodes.Count-1 do
        begin
          meta := Data.MetaData.GetByCodePos(i);
          if meta <> nil then
          begin
            stepDebugInstructions.AddItem(meta.AUnitName + '.' + meta.Name, TObject($7FFFFFFF));
          end;
          if Data.OpCodes[i].OpCode = soFLOW_CALL then
          begin
            meta := Data.MetaData.GetByCodePos(PSE2OpFLOW_CALL(Data.OpCodes[i]).Position);
            if meta <> nil then
               stepDebugInstructions.AddItem('CALL ' + meta.AUnitName + '.' + meta.Name, TObject($3FFFFFFF));
          end;
          stepDebugInstructions.AddItem('['+LeadingZero(i, minIndex)+'] ' + TSE2DebugHelper.OpCodeToStr(Data, Data.OpCodes[i]), TObject(i + 1));
        end;
      end;
    finally
      stepDebugInstructions.Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.ShowCurrentStack(Data: TSE2RunTime);
var i     : integer;
    entry : PSE2VarData;
    s     : string;
    Stack : TSE2StackTrace;
    Item  : TSE2StackItem;
begin
  stepDebugStack.Items.BeginUpdate;
  try
    stepDebugStack.Items.Clear;
    for i:=0 to Data.Stack.Size-1 do
    begin
      entry := Data.Stack.Items[i];
      s := TSE2DebugHelper.VarContentToStr(entry, Pointer(Data));
      with stepDebugStack.Items.Add do
      begin
        Caption := IntToStr(i);
        SubItems.Add(TSE2DebugHelper.VarTypeToStr(entry.AType));
        SubItems.Add('0x' + IntToHex(integer(entry^.tPointer), 8));
        SubItems.Add(s);
      end;
    end;
  finally
    stepDebugStack.Items.EndUpdate;
  end;

  stepCallStack.Items.BeginUpdate;
  try
    stepCallStack.Items.Clear;

    Stack := TSE2StackTrace.Create;
    try
      Data.GetCallStack(Stack);
      for i:=0 to Stack.Count-1 do
      begin
        Item := Stack[i];
        with stepCallStack.Items.Add do
        begin
          Caption := IntToStr(Stack.Count - i);
          SubItems.Add(Item.Name);
          SubItems.Add(Item.AUnitName);
          SubItems.Add(Item.ParamTrace);
        end;
      end;
      with stepCallStack.Items.Add do
      begin
        Caption := '0';
        SubItems.Add('[entry point]');
        SubItems.Add('');
        SubItems.Add('');
      end;
    finally
      Stack.Free;
    end;
  finally
    stepCallStack.Items.EndUpdate;
  end;
end;

{ ************************************************
  *               .....  .... ....               *
  ************************************************ }

procedure TestDirectCall(Self: TObject; RunTime: TSE2RunTime);
type
  TTest = procedure(Sender: string; Data: TObject; Code: integer; const value: string) of object;
var p: Pointer;
    t: TTest;
begin
  p := RunTime.CodeAccess.FindMethod('TTest.MyTestFunction', '', [pmIn, pmIn, pmIn, pmIn, pmIn],
        [btObject, btString, btObject, btS32, btString]);
  if p <> nil then
  begin
    t := TTest(RunTime.ScriptAsMethod(p, nil));
    t('Test', nil, 12345, 'Hallo');
  end;
end;

procedure TestRecordCall(RunTime: TSE2RunTime);
type
  TTest  = function(Rec: TPoint): TPoint of object;
  TTest2 = function(p1, p2, p3: Pointer): TPoint of object;
  TTest3 = procedure(var t: TPoint) of object;
var t, n: TPoint;
    p: Pointer;
    m: TTest;
    r: TTest2;
    r2 : TTest3;
begin
  p := RunTime.CodeAccess.FindMethod('RecordTest', '', [pmIn, pmResult], [btRecord, btRecord]);
  if p <> nil then
  begin
    t := Point(4, 2);
    m := TTest(RunTime.ScriptAsMethod(p, nil));
    n := m(t);

    if n.x > n.Y then
  end;

  p := RunTime.CodeAccess.FindMethod('RecordTest2', '', [pmIn, pmIn, pmIn, pmResult], [btPointer, btPointer, btPointer, btRecord]);
  if p <> nil then
  begin
    r := TTest2(RunTime.ScriptAsMethod(p, nil));
    n := r(nil, nil, nil);
    if n.X > n.Y then
  end;

  p := RunTime.CodeAccess.FindMethod('MyRecTest', '', [pmInOut], [btRecord]);
  if p <> nil then
  begin
    t := Point(0, 0);
    RunTime.Call(p, [@t]);

    r2 := TTest3(RunTime.ScriptAsMethod(p, nil));
    t := Point(0, 0);
    r2(t);

    if t.x > t.y then



  end;
end;

{program Project1;

uses
  ScriptTestUnit;

var t: TPoint;
begin
  MyRecTest2(t);
  
  Console.WriteLine(t.X);
  Console.WriteLine(t.Y);
  
  Console.ReadKey;
end.
}

procedure TMainForm.DoRunScript(Data: TSE2PE; Stepping: boolean);
var c, t1, t2: int64;
    p        : TSE2OpCode;
   // s        : string;
begin
  UpdateCaption(SExecutingAction);
  Run_Run.Enabled  := Stepping;
  Run_Stop.Enabled := True;

  FRunTime := TSE2RunTime.Create;
  Screen.Cursor := crAppStart;
  try          
    FCodeEditorMngr.Messages.RunTime.Clear;
    FCodeEditorMngr.Messages.RegisterRunTime(FRunTime);
    if Stepping then
      FRunTime.OnBeforeOperation := RunTimeBeforeStepCall
    else
      FRunTime.OnBeforeOperation := RunTimeBeforeCall;
    FRunTime.AppCode := Data;
    FDoAbort         := False;

    QueryPerformanceFrequency(c);
    QueryPerformanceCounter(t1);
                           
    Screen.Cursor := crDefault;
    
    FRunTime.Initialize;

    //TestRecordCall(FRunTime);
    //TestDirectCall(Self, FRunTime);

    FRunTime.Run;
    FRunTime.Finalize;

    QueryPerformanceCounter(t2);
    FCodeEditorMngr.Messages.RunTime.Messages.Lines.Add(Format(SExecutedIn, [FloatToStrF((t2 - t1) / c, ffNumber, 8, 3)]));


    for p := soNOOP to soSAFE_SJUMP do
    begin
             {
      if FRunTime.PerfMonitor[p] > 0 then
      begin
        s := TSE2DebugHelper.OpCodeToStr(p) + ': '+FloatToStrF(FRunTime.PerfMonitor[p] * 1000, ffNumber, 8, 0) + ' ms';
        s := s + ' ('+FloatToStrF(FRunTime.PerfMonitor.Count[p], ffNumber, 8, 0)+') -> ';
        s := s + FloatToStrF(FRunTime.PerfMonitor[p] * 1000 * 1000 / FRunTime.PerfMonitor.Count[p], ffNumber, 8, 3) + ' us/op';

         FCodeEditorMngr.Messages.RunTime.Messages.Lines.Add(s);

      end;
      }
    end;


    ConsoleForm.CloseConsole;
  finally
    Screen.Cursor := crDefault;
    UpdateCaption;
    Run_Run.Enabled  := True;
    Run_Stop.Enabled := False;
    FRunTime.Free;
    FRunTime := nil;
  end;
end;

procedure TMainForm.EditorRequireSave(Sender: TObject;
  CodeTab: TCodeEditorPanel; var CanClose: boolean);
begin
  case MessageDlg(Format(SSavePromt, [CodeTab.Caption]), mtConfirmation, mbYesNoCancel, 0) of
  ID_YES :
      begin
        CanClose := DoSaveUnitTab(CodeTab);
      end;
  ID_NO :
      begin
        CanClose := True;  
      end;
  ID_CANCEL :
      begin
        CanClose := False;
      end;
  end;
end;

function TMainForm.DoSaveUnitTab(Tab: TCodeEditorPanel): boolean;
begin
  result := False;
  if Tab = nil then
     exit;

  if FProjectFile.BaseFolder = '' then
  begin
    if not SaveProjectTo then
       exit;
    if FProjectFile.BaseFolder = '' then
       exit;
  end;
  result := True;

  TScriptUnit(Tab.Data).Source   := Tab.Editor.Source;
  TScriptUnit(Tab.Data).Compiled := False;

  TScriptUnit(Tab.Data).SaveToFile(FProjectFile.BaseFolder + TScriptUnit(Tab.Data).FileName);

  FProjectFile.SaveProject;  
  Tab.Modified := False;
end;

function TMainForm.SaveProjectTo: boolean;
var dlg        : TSaveDialog;
    FileName   : string;
begin
  result := False;
  dlg := TSaveDialog.Create(Self);
  try
    dlg.InitialDir := ExtractFilePath(Application.ExeName) + 'Projects';
    dlg.Filter := 'Script Projects (*.sproject)|*.sproject';
    dlg.FilterIndex := 1;
    dlg.FileName := FProjectFile.ProjectFile.FileName;
    dlg.Options := dlg.Options + [ofOverwritePrompt];

    if dlg.Execute then
    begin
      result   := True;
      FileName := dlg.FileName;
      if Pos('.', ExtractFileName(FileName)) = 0 then
         FileName := FileName + '.sproject';
      FProjectFile.SaveProject( ChangeFileExt(FileName, '.sproject'));
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.File_OpenClick(Sender: TObject);
var dlg: TOpenDialog;
begin
  if Run_Stop.Enabled then
     raise EAbort.Create('');

  if not CheckUnsavedDocuments then
     exit;

  dlg := TOpenDialog.Create(nil);
  try
    dlg.Filter := SProjectFiles + ' (*.sproject)|*.sproject';
    dlg.InitialDir := ExtractFilePath(Application.ExeName) + 'Projects';
    Screen.Cursor := crHourGlass;
    if dlg.Execute then
    begin
      Screen.Cursor := crDefault;
      DoOpenProject(dlg.FileName);
    end;
  finally
    Screen.Cursor := crDefault;
    dlg.Free;
  end;
end;

procedure TMainForm.DoOpenProject(const FileName: string);
begin             
  ProjectsAdd(FileName);

  CloseProject;
  FProjectFile.OpenProject(FileName);
  FillProjectTree;
  DoOpenUnit(FProjectFile.AProgram);
  UpdateCaption;
  
  MainMenu_Project.Enabled := True;
  MainMenu_Run.Enabled := True;
end;

procedure TMainForm.File_CloseProjectClick(Sender: TObject);
begin
  if CheckUnsavedDocuments then
     CloseProject;
end;

procedure TMainForm.EditorRequireUnit(Sender, UnitData: TObject);
begin
  DoOpenUnit(TScriptUnit(UnitData));
end;

procedure TMainForm.stepDebugInstructionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Index < TListBox(Control).Items.Count then
  begin
    if odSelected in State then
      TListBox(Control).Canvas.Brush.Color := clSkyBlue
    else
      TListBox(Control).Canvas.Brush.Color := clWhite;
    TListBox(Control).Canvas.FillRect(Rect);

    if TListBox(Control).Items.Objects[Index] = TObject($7FFFFFFF) then
    begin
      TListBox(Control).Canvas.Font.Style := [fsBold];
      TListBox(Control).Canvas.Font.Color := clRed;
    end else
    if Cardinal(TListBox(Control).Items.Objects[Index]) = $3FFFFFFF then
    begin
      TListBox(Control).Canvas.Font.Style := [fsBold];
      TListBox(Control).Canvas.Font.Color := clNavy;
    end else
    begin
      TListBox(Control).Canvas.Font.Style := [];
      TListBox(Control).Canvas.Font.Color := clBlack;
    end;
    TListBox(Control).Canvas.TextOut(Rect.Left, Rect.Top, TListBox(Control).Items[Index]);
  end;
end;

procedure TMainForm.Project_SaveToFileClick(Sender: TObject);
var PE: TSE2PE;
    dlg: TSaveDialog;
    fs : TFileStream;
begin
  PE := DoCompile;
  if PE = nil then
     exit;
  try
    dlg := TSaveDialog.Create(Self);
    try
      dlg.Filter := 'Every file|*.*';
      if dlg.Execute then
      begin
        if FileExists(dlg.FileName) then
           DeleteFile(dlg.FileName);
        fs := TFileStream.Create(dlg.FileName, fmCreate);
        try
          PE.SaveToStream(fs);
        finally
          fs.Free;
        end;
      end;
    finally
      dlg.Free;
    end;
  finally
    PE.Free;
  end;
end;


{ ************************************************
  *               Search Dialogs                 *
  ************************************************ }

var
  gbSearchBackwards: boolean;
  gbListComplete: boolean = False;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean = True;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;

  gsSearchText: string;
  gsSearchTextHistory: string; 
  gsReplaceText: string;

procedure TMainForm.ShowSearchReplaceDialog(AReplace: boolean);
var dlg: TSearchDialog;
    p  : TCodeEditorPanel;
begin
  p := FCodeEditorMngr.Editor.Selected;
  if p = nil then
     exit;

  if AReplace then
  begin
    exit;
  end;
  dlg := TSearchDialog.Create(Self);
  with dlg do
  try
    // assign search options
    ListCompleteSearch  := gbListComplete;
    SearchBackwards     := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    SearchRegularExpression := gbSearchRegex;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if p.Editor.Editor.SelAvail and (p.Editor.Editor.BlockBegin.Line = p.Editor.Editor.BlockEnd.Line)
      then
        SearchText := p.Editor.Editor.SelText
      else
        SearchText := p.Editor.Editor.GetWordAtRowCol(p.Editor.Editor.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    {if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;  }
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then
    begin
      FCodeEditorMngr.Messages.Search.Clear;
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;  
      gbListComplete := ListCompleteSearch;
      {if AReplace then with dlg as TTextReplaceDialog do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;     }
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then
      begin
        if gbListComplete then
        begin
          p.Editor.Editor.BeginUpdate;
          try
            while DoSearchReplaceText(AReplace, gbSearchBackwards) do
              fSearchFromCaret := TRUE;
          finally
            p.Editor.Editor.EndUpdate;
          end;
        end else
        begin
          DoSearchReplaceText(AReplace, gbSearchBackwards);
          fSearchFromCaret := TRUE;
        end;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

function TMainForm.DoSearchReplaceText(AReplace, ABackwards: boolean): boolean;
var Options: TSynSearchOptions;
    p      : TCodeEditorPanel;
begin
  result := False;
  p := FCodeEditorMngr.Editor.Selected;
  if p = nil then
     exit;
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if gbSearchRegex then
    p.Editor.Editor.SearchEngine := SynEditRegexSearch   
  else
    p.Editor.Editor.SearchEngine := SynEditSearch;
  if p.Editor.Editor.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    if not gbListComplete then
       MessageDlg(STextNotFound, mtInformation, [mbOK], 0);
    result := False;
    if ssoBackwards in Options then
      p.Editor.Editor.BlockEnd := p.Editor.Editor.BlockBegin
    else
      p.Editor.Editor.BlockBegin := p.Editor.Editor.BlockEnd;
    p.Editor.Editor.CaretXY := p.Editor.Editor.BlockBegin;
  end else
  begin
    if gbListComplete then
    begin
      FCodeEditorMngr.Messages.ViewType := mvSearch;

      FCodeEditorMngr.Messages.Search.Add(p.Caption, p.Editor.Editor.CaretY, p.Editor.Editor.CaretX, p.Editor.Editor.Lines[p.Editor.Editor.CaretY - 1], p);
      //Messages_Search.Items.Add(p.aCaption+' ('+IntToStr(p.pEdit.CaretY)+','+IntToStr(p.pEdit.CaretX)+') '+
      //                          p.pEdit.Lines[p.pEdit.CaretY - 1]);
    end;
    result := True;
  end;
end;

procedure TMainForm.Search_SearchClick(Sender: TObject);
begin
  ShowSearchReplaceDialog(False);   
end;

procedure TMainForm.Search_SearchNextClick(Sender: TObject);
begin
  DoSearchReplaceText(False, gbSearchBackwards);
end;

procedure TMainForm.Search_GoToLineClick(Sender: TObject);
var s: string;
    p: TCodeEditorPanel;
    i: integer;
begin
  p := FCodeEditorMngr.Editor.Selected;
  if p = nil then
  begin
    exit;
  end;

  s := IntToStr(p.Editor.Editor.CaretY);
  if InputQuery(SLineNumberTitle, SLineNumberCaption, s) then
  begin
    if not TryStrToInt(s, i) then
    begin
      MessageDlg(SInvalidNumber, mtError, [mbOK], 0);
      exit;
    end;
    if (i < 1) or (i > p.Editor.Editor.Lines.Count) then
    begin
      MessageDlg(Format(SNumberNotInRange, [p.Editor.Editor.Lines.Count]), mtError, [mbOk], 0);
      exit;
    end;


    p.Editor.Editor.GotoLineAndCenter(i);
    p.Editor.Editor.SetFocus;
  end;
end;

procedure TMainForm.packageTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
     Sender.Canvas.Font.Style := [fsBold]
  else
     Sender.Canvas.Font.Style := [];
end;

procedure TMainForm.GenUnit_PackageClick(Sender: TObject);
var Importer : TSE2ScriptImporter;
    Compiler : TSE2Compiler;
    Editor   : TCodeEditorPanel;
    Project  : TCodeEditorPanel;
    PE       : TSE2PE;
begin
  if not (Sender is TMenuItem) then
     exit;

  if FCodeEditorMngr.Editor.Selected = nil then
     exit;                                 
  Editor := FCodeEditorMngr.Editor.Selected;

  Importer := nil;
  case TMenuItem(Sender).Tag of
  1 : Importer := TSE2ScriptImporterAppPascal.Create;
  2 : Importer := TSE2ScriptImporterPackagePascal.Create;
  end;

  if Importer = nil then
     exit;

  Screen.Cursor := crHourGlass;
  try

    Compiler := TSE2Compiler.Create;
    try
      Compiler.UnitCache := FUnitCache;
      FCodeEditorMngr.Messages.Messages.Clear;
      FCodeEditorMngr.Messages.RegisterCompiler(Compiler);
      Compiler.OnGetFile := CompilerGetReader;

      Project := FCodeEditorMngr.Editor.GetByData(FProjectFile.AProgram);

      if Project <> nil then
         PE := Compiler.Compile(TSE2StringReader.Create(Project.Editor.Source, FProjectFile.AProgram))
      else
         PE := Compiler.Compile(TSE2StringReader.Create(FProjectFile.AProgram.Source, FProjectFile.AProgram));
      if PE <> nil then
      begin
        PE.Free;

        Importer.ImportSource := True;
        Importer.UnitSource   := Editor.Editor.Source;
        if Importer.Import(Compiler.UnitList, Editor.Caption) then
        begin
          FCodeEditorMngr.Editor.Add(Editor.Caption + ' [import]').Editor.Source := Importer.Output.Text;
        end;
      end;
    finally
      Compiler.Free;
    end;
  finally
    Screen.Cursor := crDefault;
    Importer.Free;
  end;
end;

procedure TMainForm.ProjectsAdd(const FileName: string);
var i    : integer;
    c    : integer;
    Item : TListItem;
begin
  c := -1;
  for i:=0 to listProjects.Items.Count-1 do
  begin
    if AnsiSameText(listProjects.Items[i].SubItems[0], FileName) then
    begin
      c := i;
      break;
    end;
  end;

  listProjects.Items.BeginUpdate;
  try                                   
    if c > -1 then
       listProjects.Items.Delete(c);

    Item := listProjects.Items.Insert(0);
    Item.Caption := ExtractFileName(FileName);
    Item.SubItems.Add(FileName);
    Item.ImageIndex := 3;
  finally
    listProjects.Items.EndUpdate;
  end;
end;

procedure TMainForm.ProjectsLoad;
var files    : TStringList;
    FileName : string;
    i        : integer;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'projects.mru';
  listProjects.Items.BeginUpdate;
  try
    listProjects.Clear;
    if FileExists(FileName) then
    begin
      files := TStringList.Create;
      try
        files.LoadFromFile(FileName);

        for i:=files.Count-1 downto 0 do
          ProjectsAdd(files[i]);
      finally
        files.Free;
      end;
    end;
  finally
    listProjects.Items.EndUpdate;
  end;
end;

procedure TMainForm.ProjectsSave;
var files    : TStringList;
    FileName : string;
    i        : integer;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'projects.mru';
  if FileExists(FileName) then
     DeleteFile(FileName);

  files := TStringList.Create;
  try              
    for i:=0 to listProjects.Items.Count-1 do
      files.Add(listProjects.Items[i].SubItems[0]);

    try
      files.SaveToFile(FileName);
    except
    end;
  finally
    files.Free;
  end;

end;

procedure TMainForm.listProjectsDblClick(Sender: TObject);
begin
  if listProjects.Selected = nil then
     exit;
     
  if AnsiSameText(listProjects.Selected.SubItems[0], FProjectFile.ProjectFile.FileName) then
     exit;

  if Run_Stop.Enabled then
     raise EAbort.Create('');


  if not CheckUnsavedDocuments then
     exit;

  DoOpenProject(listProjects.Selected.SubItems[0]);
  mainLeft.ActivePageIndex := 0;
end;

end.
