unit uStreamHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  uSE2PackageAPI;

const
  C_UnitName   = 'IO.Streams';
  C_UnitSource =

        'unit IO.Streams;'+#13#10+
        #13#10+
        'interface'+#13#10+   
        #13#10+
        'type' + #13#10 + 
        '  TStreamHelper = helper for TStream' + #13#10 + 
        '  public' + #13#10 + 
        '    /// Write an unsigned 8 bit integer to the stream' + #13#10 + 
        '    procedure WriteU8(value: byte); external;' + #13#10 + 
        '    /// Write a signed 8 bit integer to the stream' + #13#10 + 
        '    procedure WriteS8(value: shortint); external;' + #13#10 + 
        '    /// Write an unsigned 16 bit integer to the stream' + #13#10 + 
        '    procedure WriteU16(value: word); external;' + #13#10 + 
        '    /// Write a signed 16 bit integer to the stream' + #13#10 + 
        '    procedure WriteS16(value: smallint); external;' + #13#10 + 
        '    /// Write an unsigned 32 bit integer to the stream' + #13#10 + 
        '    procedure WriteU32(value: cardinal); external;' + #13#10 + 
        '    /// Write a signed 32 bit integer to the stream' + #13#10 + 
        '    procedure WriteS32(value: integer); external;' + #13#10 + 
        '    /// Write a signed 64 bit integer to the stream' + #13#10 + 
        '    procedure WriteS64(value: int64); external;' + #13#10 + 
        '    /// Write a 32 bit floating point value to the stream' + #13#10 + 
        '    procedure WriteSingle(value: single); external;' + #13#10 + 
        '    /// Write a 64 bit floating point value to the stream' + #13#10 + 
        '    procedure WriteDouble(value: double); external;' + #13#10 + 
        #13#10 + 
        '    /// Read an unsigned 8 bit integer from the stream' + #13#10 + 
        '    function  ReadU8: byte; external;' + #13#10 + 
        '    /// Read a signed 8 bit integer from the stream' + #13#10 + 
        '    function  ReadS8: shortint; external;' + #13#10 + 
        '    /// Read an unsigned 16 bit integer from the stream' + #13#10 + 
        '    function  ReadU16: word; external;' + #13#10 + 
        '    /// Read a signed 16 bit integer from the stream' + #13#10 + 
        '    function  ReadS16: smallint; external;' + #13#10 + 
        '    /// Read an unsigned 32 bit integer from the stream' + #13#10 + 
        '    function  ReadU32: cardinal; external;' + #13#10 + 
        '    /// Read a signed 32 bit integer from the stream' + #13#10 + 
        '    function  ReadS32: integer; external;' + #13#10 + 
        '    /// Read a signed 64 bit integer from the stream' + #13#10 + 
        '    function  ReadS64: int64; external;' + #13#10 + 
        '    /// Read a 32 bit floating point value from the stream' + #13#10 + 
        '    function  ReadSingle: single; external;' + #13#10 + 
        '    /// Read a 64 bit floating point value from the stream' + #13#10 + 
        '    function  ReadDouble: double; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';



procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

procedure TStreamHelper_WriteU8(Self: TStream; value: byte);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteS8(Self: TStream; value: shortint);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteU16(Self: TStream; value: word);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteS16(Self: TStream; value: smallint);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteU32(Self: TStream; value: cardinal);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteS32(Self: TStream; value: integer);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteS64(Self: TStream; value: int64);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteSingle(Self: TStream; value: single);
begin
  Self.Write(value, SizeOf(value));
end;

procedure TStreamHelper_WriteDouble(Self: TStream; value: double);
begin
  Self.Write(value, SizeOf(value));
end;

function TStreamHelper_ReadU8(Self: TStream): byte;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadS8(Self: TStream): shortint;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadU16(Self: TStream): word;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadS16(Self: TStream): smallint;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadU32(Self: TStream): cardinal;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadS32(Self: TStream): integer;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadS64(Self: TStream): int64;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadSingle(Self: TStream): single;
begin
  Self.Read(result, SizeOf(result));
end;

function TStreamHelper_ReadDouble(Self: TStream): double;
begin
  Self.Read(result, SizeOf(result));
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TStreamHelper_WriteU8  , 'TStreamHelper.WriteU8[0]');
  CallBack(Module, Data, @TStreamHelper_WriteS8  , 'TStreamHelper.WriteS8[0]');
  CallBack(Module, Data, @TStreamHelper_WriteU16  , 'TStreamHelper.WriteU16[0]');
  CallBack(Module, Data, @TStreamHelper_WriteS16  , 'TStreamHelper.WriteS16[0]');
  CallBack(Module, Data, @TStreamHelper_WriteU32  , 'TStreamHelper.WriteU32[0]');
  CallBack(Module, Data, @TStreamHelper_WriteS32  , 'TStreamHelper.WriteS32[0]');
  CallBack(Module, Data, @TStreamHelper_WriteS64  , 'TStreamHelper.WriteS64[0]');
  CallBack(Module, Data, @TStreamHelper_WriteSingle  , 'TStreamHelper.WriteSingle[0]');
  CallBack(Module, Data, @TStreamHelper_WriteDouble  , 'TStreamHelper.WriteDouble[0]');

  CallBack(Module, Data, @TStreamHelper_ReadU8  , 'TStreamHelper.ReadU8[0]');
  CallBack(Module, Data, @TStreamHelper_ReadS8  , 'TStreamHelper.ReadS8[0]');
  CallBack(Module, Data, @TStreamHelper_ReadU16  , 'TStreamHelper.ReadU16[0]');
  CallBack(Module, Data, @TStreamHelper_ReadS16  , 'TStreamHelper.ReadS16[0]');
  CallBack(Module, Data, @TStreamHelper_ReadU32  , 'TStreamHelper.ReadU32[0]');
  CallBack(Module, Data, @TStreamHelper_ReadS32  , 'TStreamHelper.ReadS32[0]');
  CallBack(Module, Data, @TStreamHelper_ReadS64  , 'TStreamHelper.ReadS64[0]');
  CallBack(Module, Data, @TStreamHelper_ReadSingle  , 'TStreamHelper.ReadSingle[0]');
  CallBack(Module, Data, @TStreamHelper_ReadDouble  , 'TStreamHelper.ReadDouble[0]');
end;

end.
