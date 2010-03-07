unit uFileAccess;

interface

uses
  uSE2PackageAPI,
  SysUtils, Classes;

const
  C_UnitName   = 'IO';
  C_UnitSource = 
        'unit IO;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'uses' + #13#10 + 
        '  Streams;' + #13#10 +
        #13#10 + 
        'type' + #13#10 + 
        '  File = partial class(TExternalObject)' + #13#10 +
        '  public' + #13#10 + 
        '    class function Create(const FileName: string): boolean; external;' + #13#10 +
        '    class function OpenRead(const FileName: string): TStream; external; overload;' + #13#10 + 
        '    class function OpenRead(const FileName: string; Share: integer): TStream; external; overload;' + #13#10 + 
        '    class function OpenWrite(const FileName: string): TStream; external; overload;' + #13#10 + 
        '    class function OpenWrite(const FileName: string; Share: integer): TStream; external; overload;' + #13#10 + 
        '    class function Delete(const FileName: string): boolean; external;' + #13#10 + 
        '    class function Rename(const OldName, NewName: string): boolean; external;' + #13#10 + 
        '    class function Exists(const FileName: string): boolean; external;' + #13#10 +
        '    class function LastModified(const Path: string): TDateTime; external;'+ #13#10 +
        '  end;' + #13#10 + 
        #13#10 + 
        '  Folder = partial class(TExternalObject)' + #13#10 +
        '  public' + #13#10 + 
        '    class function Create(const Path: string): boolean; external;' + #13#10 + 
        '    class function Delete(const Path: string): boolean; external; overload;' + #13#10 + 
        '    class function Delete(const Path: string; Recursive: boolean): boolean; external; overload;' + #13#10 + 
        '    class function Exists(const Path: string): boolean; external;' + #13#10 + 
        '    class function LastModified(const Path: string): TDateTime; external;' + #13#10 + 
        '    class function Rename(const Path, NewPath: string): boolean; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

type
  TFile = class
  public
    class function Create(const FileName: string): boolean;
    class function OpenRead(const FileName: string): TStream; overload;
    class function OpenRead(const FileName: string; Share: integer): TStream; overload;
    class function OpenWrite(const FileName: string): TStream; overload;
    class function OpenWrite(const FileName: string; Share: integer): TStream; overload;
    class function Delete(const FileName: string): boolean;
    class function Rename(const OldName, NewName: string): boolean;
    class function Exists(const FileName: string): boolean;
    class function LastModified(const FileName: string): TDateTime;
  end;

  TFolder = class
  public
    class function Create(const FolderName: string): boolean;
    class function Delete(const FolderName: string): boolean; overload;
    class function Delete(const FolderName: string; Recursive: boolean): boolean; overload;
    class function Exists(const FolderName: string): boolean;
    class function LastModified(const FolderName: string): TDateTime;
    class function Rename(const OldPath, NewPath: string): boolean;
  end;

function File_Create(Self: TFile; const FileName: string): boolean;
begin
  result := TFile.Create(FileName);
end;

function File_OpenRead(Self: TFile; const FileName: string): TStream;
begin
  result := TFile.OpenRead(FileName);
end;

function File_OpenRead1(Self: TFile; const FileName: string; Share: integer): TStream;
begin
  result := TFile.OpenRead(FileName, Share);
end;

function File_OpenWrite(Self: TFile; const FileName: string): TStream;
begin
  result := TFile.OpenWrite(FileName);
end;

function File_OpenWrite1(Self: TFile; const FileName: string; Share: integer): TStream;
begin
  result := TFile.OpenWrite(FileName, Share);
end;

function File_Delete(Self: TFile; const FileName: string): boolean;
begin
  result := TFile.Delete(FileName);
end;

function File_Rename(Self: TFile; const OldName, NewName: string): boolean;
begin
  result := TFile.Rename(OldName, NewName);
end;

function File_Exists(Self: TFile; const FileName: string): boolean;
begin
  result := TFile.Exists(FileName);
end;

function File_LastModified(Self: TFile; const FileName: string): TDateTime;
begin
  result := TFile.LastModified(FileName);
end;

function Folder_Create(Self: TFolder; const Path: string): boolean;
begin
  result := TFolder.Create(Path);
end;

function Folder_Delete(Self: TFolder; const Path: string): boolean;
begin
  result := TFolder.Delete(Path);
end;

function Folder_Delete1(Self: TFolder; const Path: string; Recursive: boolean): boolean;
begin
  result := TFolder.Delete(Path, Recursive);
end;

function Folder_Exists(Self: TFolder; const Path: string): boolean;
begin
  result := TFolder.Exists(Path);
end;

function Folder_LastModified(Self: TFolder; const Path: string): TDateTime;
begin
  result := TFolder.LastModified(Path);
end;

function Folder_Rename(Self: TFolder; const Path, NewPath: string): boolean;
begin
  result := TFolder.Rename(Path, NewPath);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @File_Create, 'File.Create[0]');
  CallBack(Module, Data, @File_OpenRead, 'File.OpenRead[0]');
  CallBack(Module, Data, @File_OpenRead1, 'File.OpenRead[1]');
  CallBack(Module, Data, @File_OpenWrite, 'File.OpenWrite[0]');
  CallBack(Module, Data, @File_OpenWrite1, 'File.OpenWrite[1]');
  CallBack(Module, Data, @File_Delete, 'File.Delete[0]');
  CallBack(Module, Data, @File_Rename, 'File.Rename[0]');
  CallBack(Module, Data, @File_Exists, 'File.Exists[0]');
  CallBack(Module, Data, @File_LastModified, 'File.LastModified[0]');
  CallBack(Module, Data, @Folder_Create, 'Folder.Create[0]');
  CallBack(Module, Data, @Folder_Delete, 'Folder.Delete[0]');
  CallBack(Module, Data, @Folder_Delete1, 'Folder.Delete[1]');
  CallBack(Module, Data, @Folder_Exists, 'Folder.Exists[0]');
  CallBack(Module, Data, @Folder_LastModified, 'Folder.LastModified[0]');
  CallBack(Module, Data, @Folder_Rename, 'Folder.Rename[0]');
end;

{ TFile }

class function TFile.Create(const FileName: string): boolean;
var aHandle: integer;
begin
  result := False;
  aHandle := FileCreate(FileName);
  if aHandle > -1 then
  begin
    result := True;
    FileClose(aHandle);
  end;
end;

class function TFile.Delete(const FileName: string): boolean;
begin
  result := DeleteFile(FileName);
end;

class function TFile.Exists(const FileName: string): boolean;
begin
  result := FileExists(FileName);
end;

class function TFile.LastModified(const FileName: string): TDateTime;
var res: integer;
begin
  res := FileAge(FileName);
  if res = -1 then
     result := 0
  else
     result := FileDateToDateTime(res);
end;

class function TFile.OpenRead(const FileName: string): TStream;
begin
  result := TFileStream.Create(FileName, fmOpenRead);
end;

class function TFile.OpenRead(const FileName: string;
  Share: integer): TStream;
begin
  result := TFileStream.Create(FileName, fmOpenRead or Share);
end;

class function TFile.OpenWrite(const FileName: string): TStream;
begin
  result := TFileStream.Create(fmOpenWrite);
end;

class function TFile.OpenWrite(const FileName: string;
  Share: integer): TStream;
begin
  result := TFileStream.Create(fmOpenWrite or Share);
end;

class function TFile.Rename(const OldName, NewName: string): boolean;
begin
  result := RenameFile(OldName, NewName);
end;

{ TFolder }

class function TFolder.Create(const FolderName: string): boolean;
begin
  result := ForceDirectories(FolderName);
end;

class function TFolder.Delete(const FolderName: string): boolean;
begin
  result := RemoveDir(FolderName);
end;

class function TFolder.Delete(const FolderName: string;
  Recursive: boolean): boolean;

  function RemoveRecursive(Path: string): boolean;
  var FileInfo: TSearchRec;
  begin
    result := False;
    if length(Path) = 0 then
       exit;

    result := True;
    if Path[length(Path)] <> '\' then
       Path := Path + '\';

    if FindFirst(Path + '*.*', faAnyFile, FileInfo) = 0 then
      repeat
        if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
        begin
          if FileInfo.Attr and faDirectory = 0 then
             result := DeleteFile(Path + FileInfo.Name)
          else
          begin
            result := RemoveRecursive(Path + FileInfo.Name + '\');
            if result then
               result := RemoveDir(Path);
          end;
        end;
      until (not result) or (FindNext(FileInfo) <> 0);
    FindClose(FileInfo);

    if result then
       result := RemoveDir(Path);
  end;

begin
  if not Recursive then
     result := RemoveDir(FolderName)
  else
  begin
    result := RemoveRecursive(FolderName);
  end;
end;

class function TFolder.Exists(const FolderName: string): boolean;
begin
  result := DirectoryExists(FolderName);
end;

class function TFolder.LastModified(const FolderName: string): TDateTime;
var FileInfo: TSearchRec;
begin
  if FindFirst(FolderName, faAnyFile, FileInfo) = 0 then
  begin
    if FileInfo.Attr and faDirectory <> 0 then
      result := FileDateToDateTime(FileInfo.Time)
    else
      result := 0;
  end else
    result := 0;
  FindClose(FileInfo);
end;

class function TFolder.Rename(const OldPath, NewPath: string): boolean;
var FileInfo: TSearchRec;
begin
  if FindFirst(NewPath, faAnyFile, FileInfo) = 0 then
  begin
    result := False;
    FindClose(FileInfo);
  end else
    result := RenameFile(OldPath, NewPath);
end;

end.
