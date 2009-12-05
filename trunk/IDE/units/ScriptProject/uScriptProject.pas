unit uScriptProject;

interface

uses
  Classes, SysUtils, uSE2Consts, JclSimpleXml;

type
  TScriptUnit = class(TObject)
  private
    FSource   : string;
    FFileName : string;
    FCompiled : boolean;
    FUnitName : string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Source   : string    read FSource    write FSource;
    property FileName : string    read FFileName  write FFileName;
    property Compiled : boolean   read FCompiled  write FCompiled;
    property UnitName : string    read FUnitName  write FUnitName;
  end;

  TScriptProjectFile = class(TObject)
  private
    FUnitList    : TStringList;
    FUnitPath    : TStringList;

    FProgramFile : string;
    FProjectName : string;

    FFileName    : string;
  public
    constructor Create;
    destructor Destroy; override;

    function  LoadFromFile(const FileName: string): boolean;
    procedure SaveToFile(const FileName: string = '');

    procedure Close;

    property FileName    : string      read FFileName      write FFileName;
    property ProjectName : string      read FProjectName   write FProjectName;
    property ProgramFile : string      read FProgramFile   write FProgramFile;
    property UnitList    : TStringList read FUnitList;
    property UnitPath    : TStringList read FUnitPath;
  end;

  TUnitEvent = procedure(Sender: TObject; UnitData: TScriptUnit) of object;

  TScriptProject = class(TObject)
  private
    FProjectFile : TScriptProjectFile;
    FProgram     : TScriptUnit;
    FUnitList    : TList;
    FBaseFolder  : string;

    FOnCloseUnit : TUnitEvent;
  protected
    function DoLoadUnit(aPath: string; const UnitName: string): TScriptUnit;
    function GetUnitN(UnitName: string): TScriptUnit;
    function GetUnitF(FileName: string): TScriptUnit;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Close;
    procedure NewProject(ProjectName: string);
    procedure OpenProject(const FileName: string);
    procedure SaveProject(const FolderName: string = '');
    procedure RemoveFolder(Name: string);
    procedure RemoveFile(const FileName: string);

    property  BaseFolder             : string             read FBaseFolder    write FBaseFolder;
    property  AProgram               : TScriptUnit        read FProgram;
    property  Files[FileName: string]: TScriptUnit        read GetUnitF;
    property  Units[UnitName: string]: TScriptUnit        read GetUnitN;
    property  ProjectFile            : TScriptProjectFile read FProjectFile;

    property  OnCloseUnit            : TUnitEvent         read FOnCloseUnit   write FOnCloseUnit;
  end;

implementation

{ TScriptUnit }

constructor TScriptUnit.Create;
begin
  inherited;
end;

destructor TScriptUnit.Destroy;
begin
  inherited;
end;

procedure TScriptUnit.LoadFromFile(const FileName: string);
var FS  : TFileStream;
    Name: string;
begin
  if FileExists(FileName) then
  begin
    try
      FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    except
      exit;
    end;
    try
      LoadFromStream(FS);
    finally
      FS.Free;
    end;
  end else
  begin
    Name   := ExtractFileName(FileName);
    Name   := Copy(Name, 1, length(Name) - length(ExtractFileExt(Name)));
    Source := 'unit ' + Name + ';'+#13#10#13#10+'interface'+#13#10#13#10+'implementation'+#13#10#13#10+'end.';
  end;
end;

procedure TScriptUnit.LoadFromStream(Stream: TStream);
begin
  SetLength(FSource, Stream.Size);
  Stream.Read(FSource[1], Stream.Size);
end;

procedure TScriptUnit.SaveToFile(const FileName: string);
var newFileName: string;
    FS : TFileStream;
begin
  ForceDirectories(ExtractFilePath(FileName));
  if FileExists(FileName) then
  begin
    newFileName := ChangeFileExt(FileName, '.~'+Copy(ExtractFileExt(FileName), 1, MaxInt));
    if FileExists(newFileName) then
       DeleteFile(newFileName);
     RenameFile(FileName, newFileName);
  end;
  try
    FS := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  except
    exit;
  end;
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TScriptUnit.SaveToStream(Stream: TStream);
begin
  Stream.Write(FSource[1], length(FSource));
end;

{ TScriptProjectFile }

procedure TScriptProjectFile.Close;
begin
  FUnitList.Clear;
  FUnitPath.Clear;
  FProgramFile := '';
  FProjectName := '';
  FFileName    := '';
end;

constructor TScriptProjectFile.Create;
begin
  inherited;
  FUnitList := TStringList.Create;
  FUnitPath := TStringList.Create;
end;

destructor TScriptProjectFile.Destroy;
begin
  FUnitList.Free;
  FUnitPath.Free;
  inherited;
end;

function TScriptProjectFile.LoadFromFile(const FileName: string): boolean;
var xmlLoader : TJclSimpleXML;
    unitRoot  : TJclSimpleXMLElem;
    i         : integer;
begin
  result := False;
  if not FileExists(FileName) then
     exit;

  xmlLoader := TJclSimpleXML.Create;
  try
    try
      xmlLoader.LoadFromFile(FileName);
      FProjectName := xmlLoader.Root.Items.ItemNamed['project'].Items.ItemNamed['info'].Items.ItemNamed['name'].Value;
      FProgramFile := xmlLoader.Root.Items.ItemNamed['project'].Items.ItemNamed['program'].Items.ItemNamed['file'].Value;

      unitRoot := xmlLoader.Root.Items.ItemNamed['project'].Items.ItemNamed['units'];
      for i:=0 to unitRoot.Items.Count-1 do
      begin
        if unitRoot.Items[i].Name = 'unit' then
        begin
          FUnitList.Add(unitRoot.Items[i].Items.ItemNamed['name'].Value);
          FUnitPath.Add(unitRoot.Items[i].Items.ItemNamed['file'].Value);
        end;
      end;

      FFileName := ExtractFileName(FileName);
      result    := True;
    except
      result := False;
    end;
  finally
    xmlLoader.Free;
  end;
end;

procedure TScriptProjectFile.SaveToFile(const FileName: string = '');
var xmlSaver  : TJclSimpleXML;
    unitRoot  : TJclSimpleXMLElem;
    i         : integer;
begin
  if FileName <> '' then
     FFileName := ExtractFileName(FileName);

  if FileExists(FileName) then
     DeleteFile(FileName);

  xmlSaver := TJclSimpleXML.Create;
  try
    xmlSaver.Root.Name := 'scriptProject';
    xmlSaver.Root.Items.Add('project').Items.Add('info').Items.Add('name', FProjectName);
    xmlSaver.Root.Items.ItemNamed['project'].Items.add('program').Items.Add('file', FProgramFile);

    unitRoot := xmlSaver.Root.Items.ItemNamed['project'].Items.Add('units');
    for i:=0 to FUnitList.Count-1 do
    begin
      with unitRoot.Items.Add('unit') do
      begin
        Items.Add('name', FUnitList[i]);
        Items.Add('file', FUnitPath[i]);
      end;
    end;

    xmlSaver.SaveToFile(FileName);
  finally
    xmlSaver.Free;
  end;

end;

{ TScriptProject }

procedure TScriptProject.Close;
var i: integer;
begin
  FProjectFile.Close;
  FreeAndNil(FProgram);

  for i:=FUnitList.Count-1 downto 0 do
    TScriptUnit(FUnitList[i]).Free;

  FUnitList.Clear;
  FBaseFolder := '';
end;

constructor TScriptProject.Create;
begin
  inherited;
  FProjectFile := TScriptProjectFile.Create;
  FUnitList    := TList.Create;
end;

destructor TScriptProject.Destroy;
begin
  Close;
  FUnitList.Free;
  FProjectFile.Free;
  inherited;
end;

function TScriptProject.DoLoadUnit(aPath: string; const UnitName: string): TScriptUnit;
var fFile : string;
begin
  fFile  := aPath;
  if Pos(':', aPath) = 0 then
     aPath := StringReplace(FBaseFolder + aPath, '\\', '\', [rfReplaceAll]);

  result := TScriptUnit.Create;
  result.FileName  := fFile;
  result.FUnitName := UnitName;
  result.LoadFromFile(aPath);
end;

function TScriptProject.GetUnitF(FileName: string): TScriptUnit;
var i: integer;
begin
  result := nil;
  for i:=FUnitList.Count-1 downto 0 do
    if SameText(TScriptUnit(FUnitList[i]).FileName, FileName) then
    begin
      result := FUnitList[i];
      exit;
    end;

  i := FProjectFile.UnitPath.IndexOf(FileName);
  if i > -1 then
  begin
    result := DoLoadUnit(FileName, FProjectFile.UnitList[i]);
    if result <> nil then
       FUnitList.Add(result);
  end;
end;

function TScriptProject.GetUnitN(UnitName: string): TScriptUnit;
var i: integer;
begin
  result := nil;
  for i:=FUnitList.Count-1 downto 0 do
    if SameText(TScriptUnit(FUnitList[i]).UnitName, UnitName) then
    begin
      result := FUnitList[i];
      exit;
    end;

  i := FProjectFile.UnitList.IndexOf(UnitName);
  if i > -1 then
  begin
    result := DoLoadUnit(FProjectFile.UnitPath[i], UnitName);
    FUnitList.Add(result);
  end;
end;

procedure TScriptProject.NewProject(ProjectName: string);
begin
  Close;
  FProjectFile.FileName     := ProjectName + '.sproject';
  FProjectFile.FProjectName := ProjectName;
  FProjectFile.FProgramFile := ProjectName + '.script';

  FProgram := TScriptUnit.Create;
  FProgram.UnitName   := '';
  FProgram.FileName   := ProjectName + '.script';
  FProgram.Source     := 'program '+ProjectName+';'+#13#10#13#10+'begin'+#13#10#13#10+'end.';
end;

procedure TScriptProject.OpenProject(const FileName: string);
begin
  Close;
  FBaseFolder := ExtractFilePath(FileName);
  if not FProjectFile.LoadFromFile(FileName) then
     exit;

  FProgram := DoLoadUnit(FProjectFile.ProgramFile, '');
end;

procedure TScriptProject.RemoveFile(const FileName: string);
var i: integer;
begin
  if length(FileName) = 0 then
     exit;

  for i:=FProjectFile.FUnitPath.Count-1 downto 0 do
  begin
    if SameText(FProjectFile.FUnitPath[i], FileName) then
    begin
      FProjectFile.UnitList.Delete(i);
      FProjectFile.UnitPath.Delete(i);
    end;
  end;

  for i:=FUnitList.Count-1 downto 0 do
  begin
    if SameText(TScriptUnit(FUnitList[i]).FileName, FileName) then
    begin
      if Assigned(FOnCloseUnit) then
         FOnCloseUnit(Self, FUnitList[i]);
      TScriptUnit(FUnitList[i]).Free;
      FUnitList.Delete(i);
    end;
  end;
end;

procedure TScriptProject.RemoveFolder(Name: string);
var i: integer;
begin
  if length(Name) = 0 then
     exit;

  if Name[length(Name)] <> '\' then
     Name := Name + '\';

  for i:=FProjectFile.FUnitPath.Count-1 downto 0 do
  begin
    if SameText(Copy(FProjectFile.FUnitPath[i], 1, length(Name)), Name) then
    begin
      FProjectFile.UnitList.Delete(i);
      FProjectFile.UnitPath.Delete(i);
    end;
  end;


  for i:=FUnitList.Count-1 downto 0 do
  begin
    if SameText(Copy(TScriptUnit(FUnitList[i]).FileName, 1, length(Name)), Name) then
    begin                            
      if Assigned(FOnCloseUnit) then
         FOnCloseUnit(Self, FUnitList[i]);
      TScriptUnit(FUnitList[i]).Free;
      FUnitList.Delete(i);
    end;
  end;
end;

procedure TScriptProject.SaveProject(const FolderName: string = '');
begin
  if FolderName <> '' then
  begin
    ForceDirectories(ExtractFilePath(FolderName));
    FBaseFolder := ExtractFilePath(FolderName);
    FProjectFile.SaveToFile(FolderName);
  end else
    FProjectFile.SaveToFile(FBaseFolder + FProjectFile.FileName);

  //
end;

end.
