unit uSE2RunCall;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2RunType, uSE2BaseTypes, uSE2PEData, uSE2OpCode, uSE2Consts;

type
  TSE2CallExternalType = record
    CallMethod : procedure(Stack: TSE2Stack; Method: Pointer; MetaData: TSE2MetaEntry; RunTime: Pointer);
  end;

var
  TSE2CallExternal : TSE2CallExternalType;

type
  PMethod = ^TMethod;
  TSE2MethodCall = class(TObject)
  private
    FRegister    : TList;
    FStack       : TList;

    FMethodPos   : Pointer;
    FCallType    : TSE2CallType;

    FFirstIsClass: boolean;
    FUseResPoint : boolean;
    FReturn1     : integer;
    FReturn2     : integer;
    FReturn3     : integer;
  public
    constructor Create(Method: Pointer; CallType: TSE2CallType); reintroduce;
    destructor Destroy; override;

    procedure AddMethod(const Method: PMethod);
    procedure AddU8(value: byte);
    procedure AddS8(value: Shortint);
    procedure AddU16(value: word);
    procedure AddS16(value: Smallint);
    procedure AddU32(value: cardinal);
    procedure AddS32(value: integer);
    procedure AddS64(value: int64);
    procedure AddSingle(value: single);
    procedure AddDouble(const value: double);
    procedure AddPointer(value: pointer);

    procedure Call;

    property  FirstIsClass : boolean read FFirstIsClass write FFirstIsClass;

    procedure AsDouble(target: pointer);
    procedure AsSingle(target: pointer);
    function  AsU32: cardinal;
    function  AsS64: int64;
    function  AsPointer: Pointer;
    function  AsString: string;
    function  EDXAsPointer: Pointer;
    procedure SetResultPointer(const ptr: pointer);
  end;

implementation

uses
  uSE2RunTime;

function CallMethod(const ACallType : TSE2CallType; const AMethodPointer : Pointer; const AParameters : array of LongWord; const AResultPointer : LongInt; const AUseResultPointer : Boolean; var ASafecallErrorCode : LongInt; SndResultData: PInteger) : LongWord; stdcall;
  type
    TLongWordArray = array of LongWord;
    TPosition      = (tpFirst, tpLast);

  function AddValue(const AParameters : array of LongWord; const AValue : LongWord; const APosition : TPosition) : TLongWordArray;
  var
    LIndex : LongInt;
  begin
    SetLength(Result, Succ(Length(AParameters)));
    for LIndex := Ord(APosition = tpFirst) to Pred(Length(AParameters) + Ord(APosition = tpFirst)) do
      Result[LIndex + Ord(APosition = tpFirst)] := AParameters[LIndex];

    if (APosition = tpFirst) then
      Result[0] := AValue
    else
      Result[High(Result)] := AValue;
  end;
var
  LESP        : LongWord;
  LLength     : LongInt;
  LParameters : Pointer;
  LSize       : LongInt;
  iSecRes     : integer;
begin
  Result := High(Result);
  ASafecallErrorCode := 0;

  if AUseResultPointer then
  begin
    case ACallType of
      callCdecl,
      callStdcall :
      begin
        Result := CallMethod(ACallType, AMethodPointer, AddValue(AParameters, AResultPointer, tpFirst), 0, false, ASafecallErrorCode, SndResultData);
      end;

      callPascal,
      callRegister :
      begin
        Result := CallMethod(ACallType, AMethodPointer, AddValue(AParameters, AResultPointer, tpLast), 0, false, ASafecallErrorCode, SndResultData);
      end;

      callSafecall :
      begin
        Result := CallMethod(callStdcall, AMethodPointer, AddValue(AParameters, AResultPointer, tpLast), 0, false, ASafecallErrorCode, SndResultData);
      end;
    end;
  end
  else
  begin
    LLength     := Length(AParameters);
    LParameters := @AParameters;
    LSize       := SizeOf(LongWord);

    if (AMethodPointer <> nil) then
    begin
      case ACallType of
        callCdecl,
        callStdcall :
        begin
          asm
            PUSH EAX                                                              // save EAX-Register
            PUSH EBX                                                              // save EBX-Register
            PUSH ECX                                                              // save ECX-Register (Count-Register)
            PUSH EDX                                                              // save EDX-Register

            MOV  LESP, ESP                                                        // save ESP-Register (Stack-Register)

            XOR  EAX, EAX                                                         // empty EAX-Register
            MOV  EBX, LParameters                                                 // set EBX-Register
            MOV  ECX, LLength                                                     // set ECX-Register (Count-Register)

          @DoTest:
            TEST ECX, ECX                                                         // test ECX-Register (Count-Register)
            JZ   @DoCall                                                          // if zero jump to DoCall

            MOV  EAX, ECX                                                         // EAX = ECX
            DEC  EAX                                                              // EAX = ECX - 1
            IMUL  EAX, LSize                                                      // EAX = (ECX - 1) * LSize
            ADD  EAX, EBX                                                         // EAX = (ECX - 1) * LSize + EBX
            PUSH [EAX]                                                            // push Parameter to Stack

            DEC  ECX                                                              // decrease ECX-Register (Count-Register)
            JMP  @DoTest                                                          // jump to DoTest

          @DoCall:
            CALL [AMethodPointer]                                                 // call Method
            MOV  Result, EAX                                                      // save Result
            MOV  iSecRes, EDX

            MOV  ESP, LESP                                                        // load ESP-Register (Stack-Register)

            POP  EDX                                                              // load EDX-Register
            POP  ECX                                                              // load ECX-Register (Count-Register)
            POP  EBX                                                              // load EBX-Register
            POP  EAX                                                              // load EAX-Register
          end;
        end;

        callPascal :
        begin
          asm
            PUSH EAX                                                              // save EAX-Register
            PUSH EBX                                                              // save EBX-Register
            PUSH ECX                                                              // save ECX-Register (Count-Register)
            PUSH EDX                                                              // save EDX-Register

            MOV  LESP, ESP                                                        // save ESP-Register (Stack-Register)

            XOR  EAX, EAX                                                         // empty EAX-Register
            MOV  EBX, LParameters                                                 // set EBX-Register
            MOV  ECX, LLength                                                     // set ECX-Register (Count-Register)

          @DoTest:
            TEST ECX, ECX                                                         // test ECX-Register (Count-Register)
            JZ   @DoCall                                                          // if zero jump to DoCall

            MOV  EAX, LLength                                                     // EAX = LLength
            SUB  EAX, ECX                                                         // EAX = LLength - ECX
            IMUL  EAX, LSize                                                      // EAX = (LLength - ECX) * LSize
            ADD  EAX, EBX                                                         // EAX = (LLength - ECX) * LSize + EBX
            PUSH [EAX]                                                            // push Parameter to Stack

            DEC  ECX                                                              // decrease ECX-Register (Count-Register)
            JMP  @DoTest                                                          // jump to DoTest

          @DoCall:
            CALL [AMethodPointer]                                                 // call Method
            MOV  Result, EAX                                                      // save Result
            MOV  iSecRes, EDX

            MOV  ESP, LESP                                                        // load ESP-Register (Stack-Register)

            POP  EDX                                                              // load EDX-Register
            POP  ECX                                                              // load ECX-Register (Count-Register)
            POP  EBX                                                              // load EBX-Register
            POP  EAX                                                              // load EAX-Register
          end;
        end;

        callRegister :
        begin
          asm
            PUSH EAX                                                              // save EAX-Register
            PUSH EBX                                                              // save EBX-Register
            PUSH ECX                                                              // save ECX-Register (Count-Register)
            PUSH EDX                                                              // save EDX-Register

            MOV  LESP, ESP                                                        // save ESP-Register (Stack-Register)

            XOR  EAX, EAX                                                         // empty EAX-Register
            MOV  EBX, LParameters                                                 // set EBX-Register
            MOV  ECX, LLength                                                     // set ECX-Register (Count-Register)

          @DoTest:
            TEST ECX, ECX                                                         // test ECX-Register (Count-Register)
            JZ   @DoCall                                                          // if zero jump to DoCall

            CMP  ECX, 3                                                           // compare ECX-Register (Count-Register) to 3
            JLE  @DoCmp                                                           // if less or equal jump to DoCmp

            MOV  EAX, LLength                                                     // EAX = LLength
            SUB  EAX, ECX                                                         // EAX = LLength - ECX
            ADD  EAX, 3                                                           // EAX = LLength - ECX + 3
            IMUL  EAX, LSize                                                      // EAX = (LLength - ECX + 3) * LSize
            ADD  EAX, EBX                                                         // EAX = (LLength - ECX + 3) * LSize + EBX
            PUSH [EAX]                                                            // push Parameter to Stack

            DEC  ECX                                                              // decrease ECX-Register (Count-Register)
            JMP  @DoTest                                                          // jump to DoTest

          @DoCmp:
            CMP  ECX, 1                                                           // compare ECX-Register (Count-Register) to 1
            JL   @DoCall                                                          // if less jump to DoCall

            MOV  EAX, 1                                                           // EAX = 1
            DEC  EAX                                                              // EAX = 1 - 1
            IMUL  EAX, LSize                                                      // EAX = (1 - 1) * LSize
            ADD  EAX, EBX                                                         // EAX = (1 - 1) * LSize + EBX
            MOV  EAX, [EAX]                                                       // move first parameter to EAX

            CMP  ECX, 2                                                           // compare ECX-Register (Count-Register) to 2
            JL   @DoCall                                                          // if less jump to DoCall

            PUSH EAX                                                              // save EAX-Register
            MOV  EAX, 2                                                           // EAX = 2
            DEC  EAX                                                              // EAX = 2 - 1
            IMUL  EAX, LSize                                                      // EAX = (2 - 1) * LSize
            ADD  EAX, EBX                                                         // EAX = (2 - 1) * LSize + EBX
            MOV  EDX, [EAX]                                                       // move second parameter to EDX
            POP  EAX                                                              // load EAX-Register

            CMP  ECX, 3                                                           // compare ECX-Register (Count-Register) to 3
            JL   @DoCall                                                          // if less jump to DoCall

            PUSH EAX                                                              // save EAX-Register
            PUSH EDX                                                              // save EDX-Register
            MOV  EAX, 3                                                           // EAX = 3
            DEC  EAX                                                              // EAX = 3 - 1
            IMUL  EAX, LSize                                                      // EAX = (3 - 1) * LSize
            ADD  EAX, EBX                                                         // EAX = (3 - 1) * LSize + EBX
            MOV  ECX, [EAX]                                                       // move third parameter to ECX
            POP  EDX                                                              // load EDX-Register
            POP  EAX                                                              // load EAX-Register

          @DoCall:
            CALL [AMethodPointer]                                                 // call Method
            MOV  Result, EAX                                                      // save Result
            MOV  iSecRes, EDX

            MOV  ESP, LESP                                                        // load ESP-Register (Stack-Register)

            POP  EDX                                                              // load EDX-Register
            POP  ECX                                                              // load ECX-Register (Count-Register)
            POP  EBX                                                              // load EBX-Register
            POP  EAX                                                              // load EAX-Register
          end;
        end;

        callSafecall :
        begin
          ASafecallErrorCode := CallMethod(callStdcall, AMethodPointer, AddValue(AParameters, LongWord(@Result), tpLast), AResultPointer, AUseResultPointer, ASafecallErrorCode, SndResultData);
        end;
      end;

      if SndResultData <> nil then
         SndResultData^ := iSecRes;

    end;
  end;
end;

{ TSE2MethodCall }

procedure TSE2MethodCall.AddDouble(const value: double);
var v1, v2: integer;
    p1    : PDouble;
begin
  p1 := @value;
  v1 := PInteger(p1)^;
  v2 := PInteger(Pointer(Integer(p1) + SizeOf(Pointer)))^;
  FStack.Add(Pointer(v2));
  FStack.Add(Pointer(v1));
end;

procedure TSE2MethodCall.AddPointer(value: pointer);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(value)
  else
     FStack.Add(value);
end;

procedure TSE2MethodCall.AddS16(value: Smallint);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.AddS32(value: integer);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.AddS64(value: int64);
var v1, v2: integer;
    p1    : PInt64;
begin
  p1 := @value;
  v1 := PInteger(p1)^;
  v2 := PInteger(Pointer(Integer(p1) + SizeOf(Pointer)))^;
  FStack.Add(Pointer(v2));
  FStack.Add(Pointer(v1));
end;

procedure TSE2MethodCall.AddS8(value: Shortint);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.AddSingle(value: single);
var v1  : integer;
    p1  : PSingle;
begin
  p1 := @value;
  v1 := PInteger(p1)^;
  FStack.Add(Pointer(v1));
end;

procedure TSE2MethodCall.AddU16(value: word);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.AddU32(value: cardinal);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.AddU8(value: byte);
begin
  if (FRegister.Count < 3) and (FCallType = callRegister) then
     FRegister.Add(Pointer(value))
  else
     FStack.Add(Pointer(value));
end;

procedure TSE2MethodCall.Call;
var Parameters : array of LongWord;
    len        : integer;
    i          : integer;

begin
  len := FRegister.Count;
  if FStack.Count > 0 then
  begin
    len := FStack.Count;
    if FCallType = callRegister then
       len := len + 3;
  end;

  if FFirstIsClass and (FUseResPoint) and (FCallType = callRegister) then
  begin
    if (len > 3) and (FRegister.Count < 3) then
    begin
      if FRegister.Count = 2 then
        FRegister.Add(Pointer(FReturn1))
      else
        FRegister.Insert(1, Pointer(FReturn1));
      //len := len - 1;
      FUseResPoint := False;
    end;
  end;

  SetLength(Parameters, len);

  if FCallType = callRegister then
  begin
    for i:=0 to FRegister.Count-1 do
      Parameters[i] := cardinal(FRegister[i]);

    for i:=0 to FStack.Count-1 do
      Parameters[i+3] := cardinal(FStack[i]);
  end else
  begin
    for i:=0 to FStack.Count-1 do
      Parameters[i] := cardinal(FStack[i]);
  end;

  FReturn3 := CallMethod(FCallType, FMethodPos, Parameters, FReturn1, FUseResPoint, i, @FReturn2);
end;

constructor TSE2MethodCall.Create(Method: Pointer; CallType: TSE2CallType);
begin
  inherited Create;
  FMethodPos := Method;
  FCallType  := CallType;

  FRegister := TList.Create;
  FStack    := TList.Create;
end;

destructor TSE2MethodCall.Destroy;
begin
  FRegister.Free;
  FStack.Free;
  inherited;
end;

procedure TSE2MethodCall.AsSingle(target: pointer);
var d: single;
begin
  asm FSTP d end;
  PSingle(target)^ := d;
end;

procedure TSE2MethodCall.AsDouble(target: pointer);
var d: double;
begin
  asm FSTP d end;
  PDouble(target)^ := d;
end;

function TSE2MethodCall.AsU32: cardinal;
begin
  result := FReturn3;
end;     

function TSE2MethodCall.AsPointer: Pointer;
begin
  result := Pointer(FReturn3);
end;

function TSE2MethodCall.AsS64: int64;
begin
  result := (Int64(FReturn2) shl 32) or (Int64(FReturn3) and $FFFFFFFF);
end;

function TSE2MethodCall.AsString: string;
begin
  result := PString(FReturn1)^;
end;

procedure TSE2MethodCall.SetResultPointer(const ptr: pointer);
begin
  FReturn1 := Integer(ptr);
  FUseResPoint := True;
end;

procedure CallMethodFunc(Stack: TSE2Stack; Method: Pointer; MetaData: TSE2MetaEntry; RunTime: Pointer);
var Caller    : TSE2MethodCall;
    ReturnVar : PSE2VarData;
    Param     : PSE2VarData;
    i         : integer;
    ParamDecl : byte;
    s         : string;
    CanUsePtr : boolean;
    MethPtr   : PMethod;
    pList     : TList;
begin
  if Method = nil then
     raise ESE2NullReferenceError.Create('External Method "'+MetaData.AUnitName+'.'+MetaData.Name+'" is not assigned');

  pList := nil;
  Caller := TSE2MethodCall.Create(Method, MetaData.CallType);
  try
    ReturnVar := nil;
    CanUsePtr := True;
    if MetaData.HasResult then
    begin
      ReturnVar := Stack[ Stack.Size-1  - MetaData.ParamCount - 1];
    end;
    Caller.FirstIsClass := MetaData.HasSelf;
    for i:=0 to MetaData.ParamCount-1 do
    begin                            
      ParamDecl := Ord(MetaData.ParamDecl[i + 1]);
      Param     := Stack [ Stack.Size-1  - MetaData.ParamCount + i];

      if TSE2ParamHelper.IsVarParam(ParamDecl) then
      begin
        case TSE2ParamHelper.GetParamType(ParamDecl) of
        btU8, btS8, btU16, btS16, btU32, btS32, btS64,
        btSingle, btDouble, btPointer, btObject,
        btRecord :
           Caller.AddPointer(Param.tPointer);
        btString, btUTF8String, btWideString, btPChar :
           Caller.AddPointer(PPointer(Param.tString)^);
        end;
      end else
      begin
        case TSE2ParamHelper.GetParamType(ParamDecl) of
        btU8          : Caller.AddU8(Param^.tu8^);
        btS8          : Caller.AddS8(Param^.ts8^);
        btU16         : Caller.AddU16(Param^.tu16^);
        btS16         : Caller.AddS16(Param^.ts16^);
        btU32         : Caller.AddU32(Param^.tu32^);
        btS32         : Caller.AddS32(Param^.ts32^);
        btS64         : Caller.AddS64(Param^.ts64^);
        btSingle      : Caller.AddSingle(Param^.tSingle^);
        btDouble      : Caller.AddDouble(Param^.tDouble^);                                               
        btString,
        btWideString,
        btUTF8String,
        btPChar       : Caller.AddPointer(PPointer(Param^.tString^)^);
        btObject,
        btPointer     : Caller.AddPointer(Pointer(Param^.tPointer^));
        btProcPtr     :
            begin
              New(MethPtr);
              if pList = nil then
                 pList := TList.Create;
              pList.Add(MethPtr);

              if Pointer(Param^.tPointer^) <> nil then
              begin
                MethPtr^ := TSE2RunTime(RunTime).ScriptAsMethod(
                            PPointer(Param^.tPointer)^,
                            PPointer( integer(Param^.tPointer) + SizeOf(Pointer) )^
                          );
                Caller.AddMethod(MethPtr);
              end else
              begin
                MethPtr.Code := nil;
                MethPtr.Data := nil;
                Caller.AddMethod(MethPtr);
              end;

              (*New(MethPtr);
              if pList = nil then
                 pList := TList.Create;
              pList.Add(MethPtr);

              if Pointer(Param^.tPointer^) <> nil then
              begin
                MethPtr^ := TSE2RunTime(RunTime).ScriptAsMethod(
                            PPointer(Param^.tPointer)^,
                            PPointer( integer(Param^.tPointer) + SizeOf(Pointer) )^
                          );
                Caller.AddPointer(MethPtr);
              end else
              begin
                MethPtr^.Code := nil;
                MethPtr^.Data := nil;
                Caller.AddPointer(MethPtr);
              end;   *)
            end;
        end;
      end;
    end;

    if CanUsePtr then
      if ReturnVar <> nil then
      begin
        s := '';
        case MetaData.ResultType of
        btString,
        btWideString,
        btUTF8String,
        btPChar         : Caller.SetResultPointer(Pointer(ReturnVar.tString^));
        btProcPtr       : Caller.SetResultPointer(Pointer(ReturnVar.tPointer));
        end;
      end;

    Caller.Call;

    if ReturnVar <> nil then
    begin
      case MetaData.ResultType of
      btU8                 : ReturnVar^.tu8^  := Caller.AsU32;
      btS8                 : ReturnVar^.ts8^  := Caller.AsU32;
      btU16                : ReturnVar^.tu16^ := Caller.AsU32;
      btS16                : ReturnVar^.ts16^ := Caller.AsU32;
      btU32                : ReturnVar^.tu32^ := Caller.AsU32;
      btS32                : ReturnVar^.ts32^ := Caller.AsU32;
      btS64                : ReturnVar^.ts64^ := Caller.AsS64;
      btSingle             : Caller.AsSingle(ReturnVar^.tSingle);
      btDouble             : Caller.AsDouble(ReturnVar^.tDouble);
      btPointer, btObject  : Pointer(ReturnVar^.tPointer^) := Caller.AsPointer;
      btProcPtr            :
          begin
            if PPointer(ReturnVar.tPointer)^ <> nil then
            begin
              if not TSE2RunTime(RunTime).MethodAsScript(PPointer(integer(ReturnVar.tPointer) + SizeOf(Pointer))^,
                   PPointer(ReturnVar.tPointer),
                   PPointer(integer(ReturnVar.tPointer) + SizeOf(Pointer))
                 ) then
                 PPointer(ReturnVar.tPointer)^ := nil;
            end;
          end;
      {$IFDEF FPC}
      btString             : PPointer(ReturnVar^.tString^)^     := Pointer(Caller.AsPointer);
      btUTF8String         : PPointer(ReturnVar^.tString^)^     := Pointer(Caller.AsPointer);
      btWideString         : PPointer(ReturnVar^.tString^)^     := Pointer(Caller.AsPointer);
      btPChar              : PPointer(ReturnVar^.tString^)^     := Pointer(Caller.AsPointer);
      {$ENDIF}
      end;
    end;

  finally
    if pList <> nil then
    begin
      for i:=pList.Count-1 downto 0 do
        Dispose(PMethod(pList[i]));
      pList.Free;
    end;

    Caller.Free;
  end;
end;

procedure RegisterCallMethod;
begin
  TSE2CallExternal.CallMethod := CallMethodFunc;
end;

function TSE2MethodCall.EDXAsPointer: Pointer;
begin
  result := Pointer(FReturn2);
end;

procedure TSE2MethodCall.AddMethod(const Method: PMethod);
begin
  FStack.Add(Method^.Data);
  FStack.Add(Method^.Code);
end;

initialization
  RegisterCallMethod;

end.
