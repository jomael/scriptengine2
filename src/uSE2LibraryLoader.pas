unit uSE2LibraryLoader;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2PEData;

type
  TSE2LibraryLoader = class
  private
    FList : TThreadList;
  protected
    function IndexOf(List: TList; Entry: TSE2MetaEntry): integer;
  public
    constructor Create;
    destructor Destroy; override;
                                        
    procedure CloseLibraries;
    function LoadEntry(Entry: TSE2MetaEntry): Pointer;
  end;

implementation


uses
  {$IFNDEF SEII_FPC}
  Windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  SysUtils;

type
  PSE2LibraryEntry = ^TSE2LibraryEntry;
  TSE2LibraryEntry = record
    Handle   : THandle;
    Meta     : TSE2MetaEntry;
  end;

{ TSE2LibraryLoader }

procedure TSE2LibraryLoader.CloseLibraries;
var i    : integer;
    list : TList;
begin
  list := FList.LockList;
  try
    for i:=list.Count-1 downto 0 do
    begin
      FreeLibrary(PSE2LibraryEntry(list.List[i]).Handle);
      Dispose(PSE2LibraryEntry(list.List[i]));
    end;
    list.Clear;
  finally
    FList.UnlockList;
  end;
end;

constructor TSE2LibraryLoader.Create;
begin
  inherited;
  FList := TThreadList.Create;
end;

destructor TSE2LibraryLoader.Destroy;
begin
  CloseLibraries;
  FList.Free;
  inherited;
end;

function TSE2LibraryLoader.IndexOf(List: TList; Entry: TSE2MetaEntry): integer;
var p: PSE2LibraryEntry;
begin
  for result := List.Count-1 downto 0 do
  begin
    p := List.List[result];
    if p.Meta = Entry then
       exit;

    if p.Meta.LibNameHash = Entry.LibNameHash then
      if LibraryNameIdentical(Entry.LibraryName, p.Meta.LibraryName) then
         exit;
  end;
  result := -1;
end;

function TSE2LibraryLoader.LoadEntry(Entry: TSE2MetaEntry): Pointer;
type
  TMethodIndex = record
    case boolean of
    True : (
      HighWord : Word;
      LowWord  : Word;
    );
    False : (
      AsPointer : Pointer;
    );
  end;
var list  : TList;
    index : integer;
    p     : PSE2LibraryEntry;
    ptr   : TMethodIndex;
begin
  list := FList.LockList;
  try
    index := IndexOf(list, Entry);
    if index >= 0 then
    begin
      p := PSE2LibraryEntry(List.List[index]);
      if p.Handle <> 0 then
      begin
        if Entry.LibIndex > -1 then
        begin
          ptr.HighWord := 0;
          ptr.LowWord  := Word(Entry.LibIndex and $FFFF);
        end else
          ptr.AsPointer := PAnsiChar(AnsiString(Entry.LibPosition));
        result := GetProcAddress(p.Handle, PAnsiChar(ptr.AsPointer));
      end
      else
         result := nil;
    end else
    begin
      New(p);
      p^.Meta := Entry;

      p^.Handle := LoadLibrary(PChar(p^.Meta.LibraryName));
      if p^.Handle <> 0 then
      begin
        if Entry.LibIndex > -1 then
        begin
          ptr.HighWord := 0;
          ptr.LowWord  := Word(Entry.LibIndex and $FFFF);
        end else
          ptr.AsPointer := PAnsiChar(AnsiString(Entry.LibPosition));
        result := GetProcAddress(p.Handle, PAnsiChar(ptr.AsPointer));
        
        list.Add(p);
      end else
      begin
        result := nil;
        Dispose(p);
      end;
    end;
  finally
    FList.UnlockList;
  end;
end;

end.
