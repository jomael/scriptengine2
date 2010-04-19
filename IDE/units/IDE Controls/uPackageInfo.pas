unit uPackageInfo;

interface

uses
  Windows, Classes, SysUtils, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  SynEdit, SynEditMiscClasses, Buttons, Graphics, uSE2UnitManager, uSE2Packages,
  SynHighlighterSE2;

type
  TIDEPackageInspector = class(TPanel)
  private
    FHorzSplitter  : TSplitter;
    FLeftPanel     : TPanel;
    FTree          : TTreeView;
    FClientPanel   : TPanel;
    FPackageSource : TSynEdit;
    FInfo          : TPanel;
    FTitleFileName : TLabel;
    FTitleGUID     : TLabel;
    FTitleModules  : TLabel;
    FTitleVersion  : TLabel;
    FInfoFileName  : TEdit;
    FInfoGUID      : TLabel;
    FInfoModules   : TLabel;
    FInfoVersion   : TLabel;
    FSizeToggle    : TSpeedButton;
    FSourceHighlight : TSynSE2Syn;

    FSizeImageList   : TImageList;
    FToggleUpIndex   : integer;
    FToggleDownIndex : integer;
  protected           
    procedure packageTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean); 
    procedure packageTreeDblClick(Sender: TObject);             
    procedure packageTreeKeyPress(Sender: TObject; var Key: Char); 
    procedure buttonToggleInfoClick(Sender: TObject);

    procedure ItemCompare(Sender: TObject; Node1, Node2: TTreeNode;
                Data: Integer; var Compare: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateComponents(TreeImages: TImageList);
    procedure FillTree;
                 
    property Tree            : TTreeView  read FTree;
    property Editor          : TSynEdit   read FPackageSource;
    property Highlighter     : TSynSE2Syn read FSourceHighlight;

    property SizeImageList   : TImageList read FSizeImageList     write FSizeImageList;
    property ToggleUpIndex   : integer    read FToggleUpIndex     write FToggleUpIndex;
    property ToggleDownIndex : integer    read FToggleDownIndex   write FToggleDownIndex;
  end;

implementation

{ TIDEPackageInspector }

procedure LoadImageForButton(Btn: TSpeedButton; ImgList: TImageList; Index: integer);
var bmp: TBitmap;
begin
  bmp := TBitmap.Create;

  ImgList.GetBitmap(Index, bmp);


  if Btn.Glyph = nil then
     Btn.Glyph := bmp
  else
  begin
    Btn.Glyph.Assign(bmp);
    bmp.Free;
  end;
end;

procedure TIDEPackageInspector.buttonToggleInfoClick(Sender: TObject);
begin
  if FInfo.Height > 26 then
  begin
    LoadImageForButton(FSizeToggle, FSizeImageList, FToggleDownIndex);
    FInfo.Height  := 26;
  end else
  begin                           
    LoadImageForButton(FSizeToggle, FSizeImageList, FToggleUpIndex);
    FInfo.Height  := 85;
  end;
end;

constructor TIDEPackageInspector.Create(AOwner: TComponent);
begin
  inherited;
  Height := 146;
  Width  := 446;
end;

procedure TIDEPackageInspector.CreateComponents(TreeImages: TImageList);
begin
  Self.BevelOuter := bvNone;

  FLeftPanel := TPanel.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align  := alLeft;
  FLeftPanel.Width  := 141;
  FLeftPanel.Left   := 0;
  FLeftPanel.BevelOuter := bvNone;

  FHorzSplitter := TSplitter.Create(Self);
  FHorzSplitter.Parent := Self;
  FHorzSplitter.Left   := 141;
  FHorzSplitter.Width  := 5;

  FTree := TTreeView.Create(FLeftPanel);
  FTree.Parent := FLeftPanel;
  FTree.Align  := alClient;
  FTree.ParentCtl3D := False;
  FTree.Ctl3D  := False;
  FTree.HideSelection := False;
  FTree.Images := TreeImages;
  FTree.Indent := 19;
  FTree.ReadOnly := True;
  FTree.OnCustomDrawItem := packageTreeCustomDrawItem;
  FTree.OnDblClick := packageTreeDblClick;
  FTree.OnKeyPress := packageTreeKeyPress;
  FTree.OnCompare  := ItemCompare;

  FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := Self;
  FClientPanel.Align  := alClient;
  FClientPanel.BevelOuter := bvNone;

  FPackageSource := TSynEdit.Create(FClientPanel);
  FPackageSource.Parent := FClientPanel;
  FPackageSource.Align  := alClient;
  FPackageSource.Color  := clWhite;
  FPackageSource.Font.Name := 'Courier New';
  FPackageSource.Font.Color := clBlack;
  FPackageSource.Gutter.BorderStyle := gbsNone;
  FPackageSource.Gutter.Font.Name := 'MS Sans Serif';
  FPackageSource.Gutter.LeftOffset := 10;
  FPackageSource.Gutter.ShowLineNumbers := True;
  FPackageSource.Gutter.Gradient := True;
  FPackageSource.Gutter.GradientStartColor := clSilver;
  FPackageSource.Gutter.GradientEndColor := RGB(245, 245, 245);
  FPackageSource.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollHintFollows, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces];
  FPackageSource.ReadOnly := True;
  FPackageSource.ScrollHintFormat := shfTopToBottom;
  FPackageSource.TabWidth := 2;
  FPackageSource.WantTabs := True;

  FInfo := TPanel.Create(FClientPanel);
  FInfo.Parent := FClientPanel;
  FInfo.Align  := alTop;
  FInfo.Height := 26;
  FInfo.BevelOuter := bvNone;

  FTitleFileName := TLabel.Create(FInfo);
  FTitleFileName.Parent := FInfo;
  FTitleFileName.Left   := 8;
  FTitleFileName.Top    := 8;
  FTitleFileName.Caption := 'File Name';

  FTitleGUID := TLabel.Create(FInfo);
  FTitleGUID.Parent := FInfo;
  FTitleGUID.Left   := 8;
  FTitleGUID.Top    := 28;
  FTitleGUID.Caption := 'GUID';

  FTitleModules := TLabel.Create(FInfo);
  FTitleModules.Parent := FInfo;
  FTitleModules.Left   := 8;
  FTitleModules.Top    := 48;
  FTitleModules.Caption := 'Modules';

  FTitleVersion := TLabel.Create(FInfo);
  FTitleVersion.Parent := FInfo;
  FTitleVersion.Left   := 8;
  FTitleVersion.Top    := 68;
  FTitleVersion.Caption := 'min. Version';

  FInfoFileName := TEdit.Create(FInfo);
  FInfoFileName.Parent := FInfo;
  FInfoFileName.AutoSize := False;
  FInfoFileName.Left   := 70;
  FInfoFileName.Top    := 6;
  FInfoFileName.Width  := 90; // 207;
  FInfoFileName.Height := 19;
  FInfoFileName.Anchors := [akLeft, akTop, akRight];
  FInfoFileName.Color := clBtnFace;
  FInfoFileName.Ctl3D := False;
  FInfoFileName.ReadOnly := True;

  FInfoGUID := TLabel.Create(FInfo);
  FInfoGUID.Parent := FInfo;
  FInfoGUID.Left := 70;
  FInfoGUID.Top := 28;

  FInfoModules := TLabel.Create(FInfo);
  FInfoModules.Parent := FInfo;
  FInfoModules.Left := 70;
  FInfoModules.Top := 48;

  FInfoVersion := TLabel.Create(FInfo);
  FInfoVersion.Parent := FInfo;
  FInfoVersion.Left := 70;
  FInfoVersion.Top := 68;

  FSizeToggle := TSpeedButton.Create(FInfo);
  FSizeToggle.Parent := FInfo;
  FSizeToggle.Left := 165;//280;
  FSizeToggle.Top := 6;
  FSizeToggle.Width := 19;
  FSizeToggle.Height := 19;
  FSizeToggle.Anchors := [akTop, akRight];
  FSizeToggle.Flat := True;
  FSizeToggle.OnClick := buttonToggleInfoClick;
  LoadImageForButton(FSizeToggle, FSizeImageList, FToggleDownIndex);

  FSourceHighlight := TSynSE2Syn.Create(Self);
  FPackageSource.Highlighter := FSourceHighlight;
end;

procedure TIDEPackageInspector.FillTree;
var Root  : TTreeNode;
    Child : TTreeNode;
    i, j  : integer;
    Item  : TSE2ManagedUnit;

  function FindRootNode(const Name: string): TTreeNode;
  var i: integer;
  begin
    for i:=0 to FTree.Items.Count-1 do
      if FTree.Items[i].Level = 0 then
        if SameText(FTree.Items[i].Text, Name) then
        begin
          result := FTree.Items[i];
          exit;
        end;
     result := FTree.Items.AddChild(nil, Item.UnitName);
     result.ImageIndex := 0;
     result.SelectedIndex := 0;
  end;

  function GetPackageName(const Input: string): string;
  begin
    result := ExtractFileName(Input);
    result := Copy(result, 1, length(result) - length(ExtractFileExt(result)));
  end;

  function IfThen(b: boolean; aTrue, aFalse: integer): integer;
  begin
    if b then
       result := aTrue
    else
       result := aFalse;
  end;

begin
  FTree.Items.BeginUpdate;
  try
    FTree.Items.Clear;

    for i:=0 to TSE2UnitManager.Instance.Count-1 do
      //if TSE2UnitManager.Instance[i] is TSE2PackageUnit then
      begin
        Item := TSE2UnitManager.Instance[i];
        if Item is TSE2PackageUnit then
        begin
          Root := FTree.Items.AddChild(nil, GetPackageName(TSE2PackageUnit(Item).FileName));
          Root.ImageIndex := 7;
          Root.SelectedIndex := 7;
          Root.Data := Item;
        end else
          Root := FindRootNode(Item.UnitName);

        for j:=0 to Item.Modules-1 do
        begin
          Child := FTree.Items.AddChild(Root, Format('[%d] %s', [ IfThen(Item is TSE2PackageUnit, j + 1, Root.Count + 1), Item.UnitNames[j]]) );
          Child.ImageIndex := IfThen(Item is TSE2PackageUnit, 6, 4);
          Child.SelectedIndex := Child.ImageIndex;
          Child.Data := Item;
        end;
      end;

      FTree.CustomSort(nil, 0, False);
  finally
    FTree.Items.EndUpdate;
  end;
end;

procedure TIDEPackageInspector.ItemCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
var Item1, Item2: TSE2ManagedUnit;

  function GetPackageName(const Input: string): string;
  begin
    result := ExtractFileName(Input);
    result := Copy(result, 1, length(result) - length(ExtractFileExt(result)));
  end;

begin
  Item1 := Node1.Data;
  Item2 := Node2.Data;

  Compare := 0;
  if (Node1.Level > 0) or (Node2.Level > 0) then
     exit;


  if Item1 <> nil then
  begin
    if Item2 <> nil then
       Compare := AnsiCompareText(Node1.Text, Node2.Text)
    else
       Compare := 1;
  end else
  begin
    if Item2 <> nil then
       Compare := -1
    else
    begin
      if AnsiSameText(Node1.Text, 'System') then
         Compare := -1
      else
      if AnsiSameText(Node2.Text, 'System') then
         Compare := 1
      else
         Compare := AnsiCompareText(Node1.Text, Node2.Text)
    end;
  end;

end;

procedure TIDEPackageInspector.packageTreeCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
     Sender.Canvas.Font.Style := [fsBold]
  else
     Sender.Canvas.Font.Style := [];
end;

procedure TIDEPackageInspector.packageTreeDblClick(Sender: TObject);
var Node: TTreeNode;
    Item: TSE2ManagedUnit;

  function GetIndex(s: string): integer;
  var iPos : integer;
  begin
    result := 0;
    iPos   := Pos(']', s);
    if iPos = 0 then
       exit;

    s := Copy(s, 1, iPos - 1);
    iPos   := Pos('[', s);
    if iPos = 0 then
       exit;

    s := Copy(s, iPos + 1, MaxInt);
    result := StrToIntDef(s, 1) - 1;
  end;

begin
  FPackageSource.BeginUpdate;
  try
    FPackageSource.Clear;

    if FTree.Selected = nil then
       exit;                  

    Node := FTree.Selected;

      Item := Node.Data;
      if Item is TSE2PackageUnit then
      begin
        FInfoFileName.Text      := TSE2PackageUnit(Item).FileName;
        FInfoGUID.Caption       := GUIDToString(TSE2PackageUnit(Item).GUID);
        FInfoModules.Caption    := IntToStr(Item.Modules);
        FInfoVersion.Caption    := Format('%d.%d.%d.%d', [TSE2PackageUnit(Item).MinVersion.Major,
                                                            TSE2PackageUnit(Item).MinVersion.Minor,
                                                            TSE2PackageUnit(Item).MinVersion.Patch,
                                                            TSE2PackageUnit(Item).MinVersion.Build]);
      end else
      begin
        //if Self.Height > 30 then
        //   buttonToggleInfoClick(nil);

        FInfoFileName.Text      := 'internal';
        FInfoGUID.Caption       := '';
        FInfoModules.Caption    := '1';
        FInfoVersion.Caption    := '';
      end;
      if Item <> nil then
         FPackageSource.Text := Item.GetUnitSource(GetIndex(Node.Text));
  finally
    FPackageSource.EndUpdate;
  end;
end;

procedure TIDEPackageInspector.packageTreeKeyPress(Sender: TObject;
  var Key: Char);
begin       
  if Key = #13 then
  begin
    Key := #0;
    packageTreeDblClick(Sender);
  end;
end;

end.
