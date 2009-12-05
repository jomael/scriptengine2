unit uPackageLoader;

interface

uses
  SysUtils, uSE2Packages, uSE2UnitManager;

procedure LoadPackages(const BaseFolder, Extension: string);

implementation

procedure LoadPackages(const BaseFolder, Extension: string);
var FileInfo: TSearchRec;
    aPackage: TSE2PackageUnit;
begin
  if FindFirst(BaseFolder + '*' + Extension, faAnyFile, FileInfo) = 0 then
    repeat
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
        if FileInfo.Attr and faDirectory = 0 then
          if SameText(ExtractFileExt(FileInfo.Name), Extension) then
          begin
            try
              aPackage := TSE2PackageUnit.Create(BaseFolder + FileInfo.Name);
              TSE2UnitManager.RegisterUnit(aPackage);
            except
            end;
          end;
    until FindNext(FileInfo) <> 0;
  FindClose(FileInfo);
end;

end.
