unit uBasicModule;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, uSE2PackageAPI;

const
  CBasicModuleName   = 'IO';
  CBasicModuleSource =
        'unit IO;'+#13#10+
        #13#10+
        'interface'+#13#10+
        #13#10+
        'type'+#13#10+
        '  TFileName   = string;'+#13#10+
        '  TFolderName = string;'+#13#10+
        #13#10+
        '  FileName = sealed partial class(TExternalObject)'+#13#10+
        '  public'+#13#10+
        '    class function GetFileName(const FileName: TFileName): TFileName; external;'+#13#10+
        '    class function GetFileNameWithoutExtension(const FileName: TFileName): TFileName; external;'+#13#10+
        '    class function GetExtension(const FileName: TFileName): TFileName; external;'+#13#10+
        '    class function GetPath(const FileName: TFileName): TFolderName; external;'+#13#10+
        '    class function GetFolder(const FileName: TFileName): TFolderName; external;'+#13#10+
        '    class function GetDrive(const FileName: TFileName): TFolderName; external;'+#13#10+
        '    class function GetRelativePath(const BaseName: TFolderName; DestName: TFileName): TFileName; external;'+#13#10+
        '    class function IsPathDelimiter(const FileName: TFileName; index: integer): boolean; external;'+#13#10+
        '    class function SameFileName(const s1, s2: TFileName): boolean; external;'+#13#10+
        '    class function CompareFileName(const s1, s2: TFileName): integer; external;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        '  TFileNameHelper = helper for TFileName'+#13#10+
        '  public'+#13#10+
        '    function FileName: TFileName;'+#13#10+
        '    function FileNameWithoutExtension: TFileName;'+#13#10+
        '    function Extension: TFileName;'+#13#10+
        '    function Path: TFileName;'+#13#10+
        '    function Folder: TFileName;'+#13#10+
        '    function Drive: TFileName;'+#13#10+
        '    function RelativePath(const BaseFolder: TFolderName): TFileName;'+#13#10+
        '    function IsPathDelimiter(index: integer): boolean;'+#13#10+
        '    function SameFileName(const Compare: TFileName): boolean;'+#13#10+
        '    function CompareFileName(const Compare: TFileName): integer;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        '  FolderName = sealed partial class(TExternalObject)'+#13#10+
        '  public'+#13#10+
        '    class function GetParentFolder(const Folder: TFolderName): TFolderName; external;'+#13#10+
        '    class function IncludeTrailingPathDelimiter(const Folder: TFolderName): TFolderName; external;'+#13#10+
        '    class function ExcludeTrailingPathDelimiter(const Folder: TFolderName): TFolderName; external;'+#13#10+
        '    class function GetRelativePath(const BaseName: TFolderName; DestName: TFolderName): TFolderName; external;'+#13#10+
        '    class function IsPathDelimiter(const FolderName: TFolderName; index: integer): boolean; external;'+#13#10+
        '    class function SameFolderName(const s1, s2: TFolderName): boolean; external;'+#13#10+
        '    class function CompareFolderName(const s1, s2: TFolderName): integer; external;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        '  TFolderNameHelper = helper for TFolderName'+#13#10+
        '  public'+#13#10+
        '    function ParentFolder: TFolderName;'+#13#10+
        '    function IncludeTrailingPathDelimiter: TFolderName;'+#13#10+
        '    function ExcludeTrailingPathDelimiter: TFolderName;'+#13#10+
        '    function RelativePath(const BaseName: TFolderName): TFolderName;'+#13#10+
        '    function IsPathDelimiter(index: integer): boolean;'+#13#10+
        '    function SameFolderName(const Compare: TFolderName): boolean;'+#13#10+
        '    function CompareFolderName(const Compare: TFolderName): integer;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        'implementation'+#13#10+
        #13#10+
        '{ TFileNameHelper }'+#13#10+
        #13#10+
        'function TFileNameHelper.FileName: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetFileName(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.FileNameWithoutExtension: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetFileNameWithoutExtension(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.Extension: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetExtension(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.Path: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetPath(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.Folder: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetFolder(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.Drive: TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetDrive(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.RelativePath(const BaseFolder: TFolderName): TFileName;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.GetRelativePath(BaseFolder, Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.IsPathDelimiter(index: integer): boolean;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.IsPathDelimiter(Self, index);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.SameFileName(const Compare: TFileName): boolean;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.SameFileName(Self, Compare);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFileNameHelper.CompareFileName(const Compare: TFileName): integer;'+#13#10+
        'begin'+#13#10+
        '  result := FileName.CompareFileName(Self, Compare);'+#13#10+
        'end;'+#13#10+
        #13#10+
        '{ TFolderNameHelper }'+#13#10+
        #13#10+
        'function TFolderNameHelper.ParentFolder: TFolderName;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.GetParentFolder(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.IncludeTrailingPathDelimiter: TFolderName;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.IncludeTrailingPathDelimiter(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.ExcludeTrailingPathDelimiter: TFolderName;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.ExcludeTrailingPathDelimiter(Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.RelativePath(const BaseName: TFolderName): TFolderName;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.GetRelativePath(BaseName, Self);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.IsPathDelimiter(index: integer): boolean;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.IsPathDelimiter(Self, index);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.SameFolderName(const Compare: TFolderName): boolean;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.SameFolderName(Self, Compare);'+#13#10+
        'end;'+#13#10+
        #13#10+
        'function TFolderNameHelper.CompareFolderName(const Compare: TFolderName): integer;'+#13#10+
        'begin'+#13#10+
        '  result := FolderName.CompareFolderName(Self, Compare);'+#13#10+
        'end;'+#13#10+
        #13#10+ 
        'end.';

procedure CBasicModuleRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

type
  TFileName = string;
  TFolderName = string;

  FileName = class(TObject)
  public
    class function GetFileName(const FileName: TFileName): TFileName;
    class function GetFileNameWithoutExtension(const FileName: TFileName): TFileName;
    class function GetExtension(const FileName: TFileName): TFileName;
    class function GetPath(const FileName: TFileName): TFolderName;
    class function GetFolder(const FileName: TFileName): TFolderName;
    class function GetDrive(const FileName: TFileName): TFolderName;
    class function GetRelativePath(const BaseName: TFolderName; DestName: TFileName): TFileName;
    class function IsPathDelimiter(const FileName: TFileName; index: integer): boolean;
    class function SameFileName(const s1, s2: TFileName): boolean;
    class function CompareFileName(const s1, s2: TFileName): integer;
  end;

  FolderName = class(TObject)
  public
    class function GetParentFolder(const Folder: TFolderName): TFolderName;
    class function IncludeTrailingPathDelimiter(const Folder: TFolderName): TFolderName;
    class function ExcludeTrailingPathDelimiter(const Folder: TFolderName): TFolderName;
    class function GetRelativePath(const BaseName: TFolderName; DestName: TFolderName): TFolderName;
    class function IsPathDelimiter(const FolderName: TFolderName; index: integer): boolean;
    class function SameFolderName(const s1, s2: TFolderName): boolean;
    class function CompareFolderName(const s1, s2: TFolderName): integer;
  end;

procedure CBasicModuleRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  { FileName Class }
  CallBack(Module, Data, @FileName.GetFileName, 'FileName.GetFileName[0]');
  CallBack(Module, Data, @FileName.GetFileNameWithoutExtension, 'FileName.GetFileNameWithoutExtension[0]');
  CallBack(Module, Data, @FileName.GetExtension, 'FileName.GetExtension[0]');
  CallBack(Module, Data, @FileName.GetPath, 'FileName.GetPath[0]');
  CallBack(Module, Data, @FileName.GetFolder, 'FileName.GetFolder[0]');
  CallBack(Module, Data, @FileName.GetDrive, 'FileName.GetDrive[0]');
  CallBack(Module, Data, @FileName.GetRelativePath, 'FileName.GetRelativePath[0]');
  CallBack(Module, Data, @FileName.IsPathDelimiter, 'FileName.IsPathDelimiter[0]');
  CallBack(Module, Data, @FileName.SameFileName, 'FileName.SameFileName[0]');
  CallBack(Module, Data, @FileName.CompareFileName, 'FileName.CompareFileName[0]');

  { FolderName Class }
  CallBack(Module, Data, @FolderName.GetParentFolder, 'FolderName.GetParentFolder[0]');
  CallBack(Module, Data, @FolderName.IncludeTrailingPathDelimiter, 'FolderName.IncludeTrailingPathDelimiter[0]');
  CallBack(Module, Data, @FolderName.ExcludeTrailingPathDelimiter, 'FolderName.ExcludeTrailingPathDelimiter[0]');
  CallBack(Module, Data, @FolderName.GetRelativePath, 'FolderName.GetRelativePath[0]');
  CallBack(Module, Data, @FolderName.IsPathDelimiter, 'FolderName.IsPathDelimiter[0]');
  CallBack(Module, Data, @FolderName.SameFolderName, 'FolderName.SameFolderName[0]');
  CallBack(Module, Data, @FolderName.CompareFolderName, 'FolderName.CompareFolderName[0]');

end;

{ FileName }

class function FileName.CompareFileName(const s1, s2: TFileName): integer;
begin
  result := SysUtils.AnsiCompareFileName(s1, s2);
end;

class function FileName.GetDrive(const FileName: TFileName): TFolderName;
begin
  result := SysUtils.ExtractFileDrive(FileName);
end;

class function FileName.GetExtension(const FileName: TFileName): TFileName;
begin
  result := SysUtils.ExtractFileExt(FileName);
end;

class function FileName.GetFileName(const FileName: TFileName): TFileName;
begin
  result := SysUtils.ExtractFileName(FileName);
end;

class function FileName.GetFileNameWithoutExtension(
  const FileName: TFileName): TFileName;
begin
  result := SysUtils.ExtractFileName(FileName);
  result := Copy(result, 1, length(result) - length(SysUtils.ExtractFileExt(result)));
end;

class function FileName.GetFolder(const FileName: TFileName): TFolderName;
begin
  result := SysUtils.ExtractFileDir(FileName);
end;

class function FileName.GetPath(const FileName: TFileName): TFolderName;
begin
  result := SysUtils.ExtractFilePath(FileName);
end;

class function FileName.GetRelativePath(const BaseName: TFolderName;
  DestName: TFileName): TFileName;
begin
  result := SysUtils.ExtractRelativePath(BaseName, DestName);
end;

class function FileName.IsPathDelimiter(const FileName: TFileName;
  index: integer): boolean;
begin
  result := SysUtils.IsPathDelimiter(FileName, index);
end;

class function FileName.SameFileName(const s1, s2: TFileName): boolean;
begin
  result := SysUtils.SameFileName(s1, s2);
end;

{ FolderName }

class function FolderName.CompareFolderName(const s1,
  s2: TFolderName): integer;
begin
  result := SysUtils.AnsiCompareFileName(s1, s2);
end;

class function FolderName.ExcludeTrailingPathDelimiter(
  const Folder: TFolderName): TFolderName;
begin
  result := SysUtils.ExcludeTrailingPathDelimiter(Folder);
end;

class function FolderName.GetParentFolder(
  const Folder: TFolderName): TFolderName;
begin
  result := SysUtils.ExcludeTrailingPathDelimiter(Folder);
  result := SysUtils.ExtractFileDir(result);
end;

class function FolderName.GetRelativePath(const BaseName: TFolderName;
  DestName: TFolderName): TFolderName;
begin
  result := SysUtils.ExtractRelativePath(BaseName, DestName);
end;

class function FolderName.IncludeTrailingPathDelimiter(
  const Folder: TFolderName): TFolderName;
begin
  result := SysUtils.IncludeTrailingPathDelimiter(Folder);
end;

class function FolderName.IsPathDelimiter(const FolderName: TFolderName;
  index: integer): boolean;
begin
  result := SysUtils.IsPathDelimiter(FolderName, index);
end;

class function FolderName.SameFolderName(const s1,
  s2: TFolderName): boolean;
begin
  result := SysUtils.SameFileName(s1, s2);
end;

end.
