unit uSE2SafeBlockMngr;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Consts, uSE2BaseTypes;

type
  TSE2BlockType = (blFinally, blExcept);

  TSE2TryBlock = class(TObject)
  private
    FBlockType    : TSE2BlockType;
    FIsAquired    : boolean;
    FNewStackSize : integer;
    FBlockStart   : integer;
    FBlockLeave   : integer;
    FExitTo       : integer;

    FErrorExcept  : ExceptClass;
    FErrorMsg     : string;
    FErrorStack   : string;
    FErrorPos     : integer;
  public
    property BlockType    : TSE2BlockType  read FBlockType    write FBlockType;
    property IsAquired    : boolean        read FIsAquired    write FIsAquired;
    property NewStackSize : integer        read FNewStackSize write FNewStackSize;
    property BlockStart   : integer        read FBlockStart   write FBlockStart;
    property BlockLeave   : integer        read FBlockLeave   write FBlockLeave;
    property ExitTo       : integer        read FExitTo       write FExitTo;

    property ErrorExcept  : ExceptClass    read FErrorExcept  write FErrorExcept;
    property ErrorMsg     : string         read FErrorMsg     write FErrorMsg;
    property ErrorStack   : string         read FErrorStack   write FErrorStack;
    property ErrorPos     : integer        read FErrorPos     write FErrorPos;
  end;

  TSE2SafeBlockMngr = class(TSE2Object)
  private
    FBlockList : TList;
  protected
    function GetCount: integer;
    function GetItem(index: integer): TSE2TryBlock;
    function GetTop: TSE2TryBlock;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    function  Add(aType: TSE2BlockType; StackSize, SafePos, LeavePos: integer): TSE2TryBlock;
    function  Delete(index: integer): boolean;
    procedure Pop;

    property  Items[index: integer]: TSE2TryBlock     read GetItem; default;
    property  TopItem              : TSE2TryBlock     read GetTop;
    property  Count                : integer          read GetCount;
  end;

implementation

{ TSE2SafeBlockMngr }

function TSE2SafeBlockMngr.Add(aType: TSE2BlockType; StackSize, SafePos,
  LeavePos: integer): TSE2TryBlock;
begin
  result := TSE2TryBlock.Create;
  result.BlockType     := aType;
  result.NewStackSize  := StackSize;
  result.BlockStart    := SafePos;
  result.BlockLeave    := LeavePos;
  result.ExitTo        := -1;
  result.ErrorPos      := -1;
  FBlockList.Add(result);
end;

procedure TSE2SafeBlockMngr.Clear;
var i: integer;
begin
  for i:=FBlockList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2SafeBlockMngr.Create;
begin
  inherited;
  FBlockList := TList.Create;
end;

function TSE2SafeBlockMngr.Delete(index: integer): boolean;
begin
  if (index < 0) or (index >= FBlockList.Count) then
     result := False
  else
  begin
    TSE2TryBlock(FBlockList[index]).Free;
    FBlockList.Delete(index);
    result := True;
  end;
end;

destructor TSE2SafeBlockMngr.Destroy;
begin
  Clear;
  FBlockList.Free;
  inherited;
end;

function TSE2SafeBlockMngr.GetCount: integer;
begin
  result := FBlockList.Count;
end;

function TSE2SafeBlockMngr.GetItem(index: integer): TSE2TryBlock;
begin
  if (index < 0) or (index >= FBlockList.Count) then
     result := nil
  else
     result := FBlockList[index];
end;

function TSE2SafeBlockMngr.GetTop: TSE2TryBlock;
begin
  result := GetItem(FBlockList.Count-1);
end;

procedure TSE2SafeBlockMngr.Pop;
begin
  Delete(FBlockList.Count-1);
end;

end.
