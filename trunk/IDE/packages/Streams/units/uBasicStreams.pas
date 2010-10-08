unit uBasicStreams;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  uSE2PackageAPI;

const
  CBasicStreamsName   = 'IO.Streams';
  CBasicStreamsSource =

        'unit IO.Streams;'+#13#10+
        #13#10+
        'interface'+#13#10+   
        #13#10+
        'type'+#13#10+
        '  TSeekOrigin = (soBeginning, soCurrent, soEnd);'+#13#10+ 
        #13#10+
        '  EStreamError = class(EExternalException)'+#13#10+
        '  public'+#13#10+
        '    constructor Create(const Message: string); override;'+#13#10+
        '  end;'+#13#10+
        #13#10 +
        '  /// Gives the possibility to read or write data from or to a data stream' + #13#10 + 
        '  TStream = class(TExternalObject)' + #13#10 + 
        '  private' + #13#10 + 
        '    function GetPosition: int64; external;' + #13#10 + 
        '    function GetSize: int64; external;' + #13#10 + 
        #13#10 + 
        '    procedure SetPosition(value: int64); external;' + #13#10 + 
        '    procedure SetSize(value: int64); external;' + #13#10 + 
        #13#10 + 
        '    function  GetCanRead: boolean; external;' + #13#10 + 
        '    function  GetCanWrite: boolean; external;' + #13#10 + 
        '    function  GetCanSeek: boolean; external;' + #13#10 + 
        '  public' + #13#10 + 
        '    /// Read data and store it into "Target". "Count" specifies the number of bytes to read from the stream.' + #13#10 + 
        '    /// The method returns the number of bytes, which has been written to the target buffer' + #13#10 + 
        '    function Read(Target: Pointer; Count: integer): integer; external;' + #13#10 + 
        '    /// Write data from the "Source" buffer into the stream. "Count" specifies the number of bytes to write from the buffer.' + #13#10 + 
        '    /// The method returns the number of bytes, which has been written to the stream' + #13#10 + 
        '    function Write(Source: Pointer; Count: integer): integer; external;' + #13#10 + 
        '    /// Set the position marker to a specific position. The Origin specifies the start position for the offset' + #13#10 + 
        '    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; external;' + #13#10 + 
        '    /// Copy data from another stream to this stream. "Count" specifies the number of bytes to copy.' + #13#10 + 
        '    /// If "Count" is zero, the source stream position is set to zero and the complete source stream is copied' + #13#10 + 
        '    /// to the curren stream' + #13#10 + 
        '    function CopyFrom(Source: TStream; Count: Int64): Int64; external;' + #13#10 + 
        #13#10 + 
        '    /// Write a system string to the stream. The stream length is not written to the stream' + #13#10 + 
        '    function WriteString(const s: string): integer; external;' + #13#10 + 
        '    /// Write a utf-16 string to the stream. The stream length is not written to the stream' + #13#10 + 
        '    function WriteWideString(const s: WideString): integer; external;' + #13#10 + 
        '    /// Read out a system string from the stream. The length of the string inside the stream must be specified in the "len" parameter' + #13#10 + 
        '    function ReadString(len: integer; var s: string): integer; external;' + #13#10 + 
        '    /// Read out a utf-16 string from the stream. The length of the string inside the stream must be specified in the "len" parameter' + #13#10 + 
        '    function ReadWideString(len: integer; var s: WideString): integer; external;' + #13#10 + 
        #13#10 + 
        '    /// The current position inside the stream' + #13#10 + 
        '    property Position  : Int64   read GetPosition write SetPosition;' + #13#10 + 
        '    /// The complete size of the stream' + #13#10 + 
        '    property Size      : Int64   read GetSize     write SetSize;' + #13#10 + 
        #13#10 + 
        '    property CanRead   : boolean read GetCanRead;' + #13#10 + 
        '    property CanWrite  : boolean read GetCanWrite;' + #13#10 + 
        '    property CanSeek   : boolean read GetCanSeek;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10+
        'implementation'+#13#10+ 
        #13#10 +                  
        'constructor EStreamError.Create(const Message: string);' + #13#10 +
        'begin inherited; end;' + #13#10 +
        #13#10+ 
        'end.';


procedure CBasicStreamsRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);  
procedure RegisterExceptions(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageExceptionsReg);

implementation

function Stream_GetPosition(Self: TStream): int64;
begin
  result := Self.Position;
end;

function Stream_GetSize(Self: TStream): int64;
begin
  result := Self.Size;
end;

procedure Stream_SetPosition(Self: TStream; value: int64);
begin
  Self.Position := value;
end;  

procedure Stream_SetSize(Self: TStream; value: int64);
begin
  Self.Size := value;
end;

function  Stream_Read(Self: TStream; Target: Pointer; Count: integer): integer;
begin
  result := Self.Read(Target^, Count);
end;

function  Stream_Write(Self: TStream; Source: Pointer; Count: integer): integer;
begin
  result := Self.Write(Source^, Count);
end;

function  Stream_Seek(Self: TStream; Offset: int64; Origin: TSeekOrigin): int64;
begin
  result := Self.Seek(Offset, Origin);
end;

function  Stream_CopyFrom(Self: TStream; Source: TStream; Count: int64): int64;
begin
  result := Self.CopyFrom(Source, Count);
end;

function  Stream_WriteString(Self: TStream; const s: string): integer;
begin
  result := Self.Write(s[1], length(s) * SizeOf(char));
end;

function  Stream_WriteWideString(Self: TStream; const s: WideString): integer;
begin
  result := Self.Write(s[1], length(s) * SizeOf(WideChar));
end;

function  Stream_ReadString(Self: TStream; len: integer; var s: string): integer;
begin
  SetLength(s, len);
  result := Self.Read(s[1], len * SizeOf(char));
end;     

function  Stream_ReadWideString(Self: TStream; len: integer; var s: WideString): integer;
begin
  SetLength(s, len);
  result := Self.Read(s[1], len * SizeOf(WideChar));
end;

function  Stream_GetCanRead(Stream: TStream): boolean;
begin
  result := True;
end;

function  Stream_GetCanWrite(Stream: TStream): boolean;
begin
  result := True;
end;

function  Stream_GetCanSeek(Stream: TStream): boolean;
begin
  result := True;
end;

procedure CBasicStreamsRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @Stream_GetPosition, 'TStream.GetPosition[0]');
  CallBack(Module, Data, @Stream_GetSize, 'TStream.GetSize[0]');
  CallBack(Module, Data, @Stream_SetPosition, 'TStream.SetPosition[0]');
  CallBack(Module, Data, @Stream_GetSize, 'TStream.SetSize[0]');
  
  CallBack(Module, Data, @Stream_GetCanRead, 'TStream.GetCanRead[0]');
  CallBack(Module, Data, @Stream_GetCanWrite, 'TStream.GetCanWrite[0]');
  CallBack(Module, Data, @Stream_GetCanSeek, 'TStream.GetCanSeek[0]');

  CallBack(Module, Data, @Stream_Read, 'TStream.Read[0]');
  CallBack(Module, Data, @Stream_Write, 'TStream.Write[0]');
  CallBack(Module, Data, @Stream_Seek, 'TStream.Seek[0]');
  CallBack(Module, Data, @Stream_CopyFrom, 'TStream.CopyFrom[0]');
  CallBack(Module, Data, @Stream_WriteString, 'TStream.WriteString[0]');
  CallBack(Module, Data, @Stream_WriteWideString, 'TStream.WriteWideString[0]');
  CallBack(Module, Data, @Stream_ReadString, 'TStream.ReadString[0]');
  CallBack(Module, Data, @Stream_ReadWideString, 'TStream.ReadWideString[0]');   
end;                     

procedure RegisterExceptions(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageExceptionsReg);
begin
  CallBack(Module, Data, EStreamError, 'EStreamError');
end;

end.
