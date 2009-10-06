unit uSE2NativeScriptCall;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Consts, uSE2BaseTypes, uSE2NativeCallList;

procedure SE2MethodScriptCallHandler;

implementation

uses
  uSE2RunTime, uSE2OpCode, uSE2PEData, uSE2SystemUnit, uSE2RunType;

function InternalMethodHandler(MethodPtr: Pointer; const AStackPtr: Pointer; _EDX: Pointer): integer; forward;

procedure SE2MethodScriptCallHandler;
asm

    // save base pointer
    push ebp
    mov ebp, esp

    // push data registers to stack
    push ecx
    push edx

    // move stack pointer to edx
    mov edx, esp

    // call
    CALL InternalMethodHandler

    // save real result
    mov ecx, [edx]
    pop edx
    pop edx

    // restore base pointer
    mov esp, ebp
    pop ebp

    mov ebx, edx   

    // save return address
    mov edx, [esp]
    // pop stack variables
    add esp, eax
    // push return address
    mov [esp], edx

    // set real result
    mov eax, ecx
    mov edx, ebx

    ret
end;


procedure PutOnFPUStackExtended(ft: extended);
asm
//  fstp tbyte ptr [ft]
  fld tbyte ptr [ft]

end;

{$Warnings on}
function InternalMethodHandler(MethodPtr: Pointer; const AStackPtr: Pointer; _EDX: Pointer): integer;
var CallInfo     : PSE2NativeCallEntry;
    RunTime      : TSE2RunTime;
    ClassPtr     : Pointer;
    ScriptMethod : TSE2MetaEntry;
    OldCodePos   : integer;       
    Methods      : TSE2DynMethodList;
    Index        : integer;

  // StackPtr + Ptr * 0 = EAX = MethodPtr
  // StackPtr + Ptr * 1 = EDX = 1st parameter
  // StackPtr + Ptr * 2 = ECX = 2nd parameter
  // StackPtr + Ptr * 5 = Last Parameter on the stack
  // StackPtr + Ptr * 6 = Last Parameter - 1 on the stack
  //  ...
  //
  // Register: call order
  // Stack: reverse call order

  function  SupportsRegister(ParamType: TSE2TypeIdent): boolean;
  begin
    result :=
      ParamType in
        [btU8, btS8, btU16, btS16, btU32, btS32,
         btPointer, btObject, btRecord,
         btString, btUTF8String, btWideString, btPChar];
  end;

  function  PointerSize(ParamType: TSE2TypeIdent; isVarParam: boolean): integer;
  begin
    result := 1;
    if isVarParam then
       exit;
    case ParamType of
    btDouble,
    btS64 :
        result := 2;
    end;
  end;

  function  ParamsInStack(Meth: TSE2MetaEntry): integer;
  var i           : integer;
      Start       : integer;  
      Registers   : integer;
      aParamType  : TSE2TypeIdent;
      bIsVarParam : boolean;
  begin
    Start     := 0;
    Registers := 1;
    result    := 0;
    if ScriptMethod.HasSelf then
    begin
      Start := 1;
    end;

    for i:=Start to Meth.ParamCount-1 do
    begin
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(ScriptMethod.ParamDecl[i+1]));
      aParamType  := TSE2ParamHelper.GetParamType(Ord(ScriptMethod.ParamDecl[i+1]));

      if (Registers < 4 - PointerSize(aParamType, bIsVarParam)) and (SupportsRegister(aParamType) or bIsVarParam) then
         Registers := Registers + PointerSize(aParamType, bIsVarParam)
      else
         result := Result + PointerSize(aParamType, bIsVarParam);
    end;
  end;

  function  GetParameterData(index: integer): pointer;
  var i          : integer;
      Start      : integer;
      Registers  : integer;
      StackPtr   : Pointer;
      RegPtr     : Pointer;
      PtrOffset  : integer;
      aParamType : TSE2TypeIdent;
      bIsVarParam: boolean;
      StackParams: integer;
  begin
    Start     := 0;
    Registers := 1;
    if ScriptMethod.HasSelf then
    begin
      Start := 1;
    end;

    StackParams := ParamsInStack(ScriptMethod);

    PtrOffset := 3;
    if ScriptMethod.HasResult then
      if ScriptMethod.ResultType in [btString, btUTF8String, btWideString] then
        PtrOffset := 4;

    RegPtr   := Pointer(integer(AStackPtr) + SizeOf(Pointer) * 0);
    StackPtr := Pointer(integer(AStackPtr) + SizeOf(Pointer) * (StackParams + PtrOffset));

    result := StackPtr;
    for i:=Start to ScriptMethod.ParamCount-1 do
    begin
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(ScriptMethod.ParamDecl[i+1]));
      aParamType  := TSE2ParamHelper.GetParamType(Ord(ScriptMethod.ParamDecl[i+1]));

      if i = index then
      begin
        if (Registers < 4 - PointerSize(aParamType, bIsVarParam)) and (SupportsRegister(aParamType) or bIsVarParam) then
           result := RegPtr
        else
           result := StackPtr;
        exit;
      end;

      if (Registers < 4 - PointerSize(aParamType, bIsVarParam)) and (SupportsRegister(aParamType) or bIsVarParam) then
      begin
        Registers := Registers + PointerSize(aParamType, bIsVarParam);
        RegPtr    := Pointer(integer(RegPtr) + SizeOf(Pointer) * PointerSize(aParamType, bIsVarParam));
      end
      else
      begin
        StackPtr  := Pointer(integer(StackPtr) - SizeOf(Pointer) * PointerSize(aParamType, bIsVarParam));
      end;
    end;
                (*
    if index < 2 then
    begin
      result := Pointer(cardinal(StackPtr) + SizeOf(Pointer) * index);
    end else
    begin
      result := Pointer(cardinal(StackPtr) + SizeOf(Pointer) * 5);
      result := Pointer(cardinal(result) + SizeOf(Pointer) * (ScriptMethod.ParamCount - index - 2));
    end;   *)
  end;

  procedure SetVariableContent(aParamType: byte; DataType: byte; Data: Pointer);
  const 
    SParamNotCompatible = 'Parameter not compatible to script method parameter';

  var newEntry    : PSE2VarData;
      fSingle     : single;
      fDouble     : double;
  begin
    case DataType of
    vtInteger :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btU8      : newEntry.tu8^      := PInteger(Data)^;
          btS8      : newEntry.ts8^      := PInteger(Data)^;
          btU16     : newEntry.tu16^     := PInteger(Data)^;
          btS16     : newEntry.ts16^     := PInteger(Data)^;
          btU32     : newEntry.tu32^     := PInteger(Data)^;
          btS32     : newEntry.ts32^     := PInteger(Data)^;
          btS64     : newEntry.ts64^     := PInteger(Data)^;
          btSingle  : newEntry.tSingle^  := PInteger(Data)^;
          btDouble  : newEntry.tDouble^  := PInteger(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtInt64 :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btU8      : newEntry.tu8^      := PInt64(Data)^;
          btS8      : newEntry.ts8^      := PInt64(Data)^;
          btU16     : newEntry.tu16^     := PInt64(Data)^;
          btS16     : newEntry.ts16^     := PInt64(Data)^;
          btU32     : newEntry.tu32^     := PInt64(Data)^;
          btS32     : newEntry.ts32^     := PInt64(Data)^;
          btS64     : newEntry.ts64^     := PInt64(Data)^;
          btSingle  : newEntry.tSingle^  := PInt64(Data)^;
          btDouble  : newEntry.tDouble^  := PInt64(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtExtended :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btSingle  :
              begin
                fSingle := PSingle(Data)^;
                newEntry.tSingle^  := fSingle;
              end;
          btDouble :
              begin
                fDouble := PDouble(Data)^;
                newEntry.tDouble^  := fDouble;
              end;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtCurrency :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btSingle  :
              begin
                fSingle := PCurrency(Data)^;
                newEntry.tSingle^  := fSingle;
              end;
          btDouble :
              begin
                fDouble := PCurrency(Data)^;
                newEntry.tDouble^  := fDouble;
              end;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtBoolean :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btBoolean : newEntry.tu8^ := Ord(PBoolean(Data)^);
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtString :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtAnsiString :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtPChar:
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtChar :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtWideString :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^)));
          btWideString : PbtWideString(newEntry.tString^)^ := WideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := UTF8Encode(WideString(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^))));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtPointer :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtClass :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtObject :
        begin
          newEntry := RunTime.Stack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    else raise ESE2CallParameterError.Create('Unsupported parameter');
    end;
  end;

  procedure PushParamsToStack;
  var i           : integer;
      bIsVarParam : boolean;
      aParamType  : byte;
      Parameter   : Pointer;
  begin
    // result
    if ScriptMethod.HasResult then
       RunTime.Stack.PushNew(ScriptMethod.ResultType);

    // parameters
    for i:=0 to ScriptMethod.ParamCount-1 do
    begin
      Parameter   := GetParameterData(i);
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(ScriptMethod.ParamDecl[i+1]));
      aParamType  := TSE2ParamHelper.GetParamType(Ord(ScriptMethod.ParamDecl[i+1]));

      if ScriptMethod.HasSelf and (i = 0) then
      begin
         SetVariableContent(aParamType, vtObject, @ClassPtr);
         continue;
      end;

      if not bIsVarParam then
      begin
        case aParamType of
        btU8, btS8, btU16, btS16, btU32, btS32 :
            SetVariableContent(aParamType, vtInteger, Parameter);
        btS64 :
            SetVariableContent(aParamType, vtInt64, Parameter);
        btSingle :
            SetVariableContent(aParamType, vtExtended, Parameter);
        btDouble :
            SetVariableContent(aParamType, vtExtended, Pointer(integer(Parameter) - SizeOf(Pointer)));
        btString, btUTF8String :
            SetVariableContent(aParamType, vtAnsiString, @Parameter);
        btPChar :
            SetVariableContent(aParamType, vtPChar, @Parameter);
        btChar :
            SetVariableContent(aParamType, vtChar, @Parameter);
        btWideString :
            SetVariableContent(aParamType, vtWideString, Parameter);
        btPointer :
            SetVariableContent(aParamType, vtPointer, Parameter);
        btObject :
            SetVariableContent(aParamType, vtObject, Parameter);
        end;
      end else
      begin
        case aParamType of
        btU8, btS8, btU16, btS16, btU32, btS32 :
            SetVariableContent(aParamType, vtInteger, Parameter);
        btS64 :
            SetVariableContent(aParamType, vtInt64, Parameter);
        btSingle, btDouble :
            SetVariableContent(aParamType, btExtended, Parameter);
        btString :
            SetVariableContent(aParamType, vtAnsiString, Parameter);
        btUTF8String :
            SetVariableContent(aParamType, vtAnsiString, Parameter);
        btWideString :
            SetVariableContent(aParamType, vtWideString, Parameter);
        btPChar :
            SetVariableContent(aParamType, vtAnsiString, Parameter);
        btPointer, btRecord, btArray, btObject :
            SetVariableContent(aParamType, vtPointer, Parameter);
        end;
      end;
    end;

    // return address
    RunTime.Stack.PushNew(btReturnAddress)^.ts64^ := int64($FFFFFFFF00000000);
  end;

  function PopParamsFromStack: integer;
  var i           : integer;
      bIsVarParam : boolean;
      Parameter   : Pointer;
      Data        : PSE2VarData;
  begin
    result := 0;
    // return address
    // already pop-ed by OP_FLOW_RET

    // parameters
    for i:=ScriptMethod.ParamCount-1 downto 0 do
    begin
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(ScriptMethod.ParamDecl[i+1]));
      if bIsVarParam then
      begin
        Parameter   := GetParameterData(i);
        Data := RunTime.Stack.Top;
        case Data^.AType of
        btU8           : PbtU8(Parameter)^     := Data^.tU8^;
        btS8           : PbtS8(Parameter)^     := Data^.tS8^;
        btU16          : PbtU16(Parameter)^    := Data^.tU16^;
        btS16          : PbtS16(Parameter)^    := Data^.tS16^;
        btU32          : PbtU32(Parameter)^    := Data^.tU32^;
        btS32          : PbtS32(Parameter)^    := Data^.tS32^;
        btS64          : PbtS64(Parameter)^    := Data^.tS64^;
        btSingle       : PbtSingle(Parameter)^ := Data^.tSingle^;
        btDouble       : PbtDouble(Parameter)^ := Data^.tDouble^;
        btString       : PbtString(Parameter)^ := PbtString(Data^.tString^)^;
        btUTF8String   : PbtUTF8String(Parameter)^ := PbtUTF8String(Data^.tString^)^;
        btWideString   : PbtWideString(Parameter)^ := PbtWideString(Data^.tString^)^;
        btPChar        : PbtPChar(Parameter)^      := PbtPChar(Data^.tString^)^;
        btPointer,
        btArray,
        btRecord       : PPointer(Parameter)^    := Pointer(Data^.tPointer^);
        end;
      end;

      RunTime.Stack.Pop;
    end;

    // result
    if ScriptMethod.HasResult then
    begin
      Data := RunTime.Stack.Top;
      case Data^.AType of
      btU8         : result := Data^.tU8^;
      btS8         : result := Data^.tS8^;
      btU16        : result := Data^.tU16^;
      btS16        : result := Data^.tS16^;
      btU32        : result := Data^.tU32^;
      btS32        : result := Data^.tS32^;

      btSingle     : PutOnFPUStackExtended(Data^.tSingle^);
      btDouble     : PutOnFPUStackExtended(Data^.tDouble^);

      btString     :
          begin
            {$IFDEF FPC}
            result := integer(PPointer(Data^.tString^)^);
            {$ELSE}
            Parameter := pointer(integer(AStackPtr) + SizeOf(Pointer) * 0 );
            PbtString(Parameter^)^ := PbtString(Data^.tString^)^;
            {$ENDIF}
          end;
      btUTF8String :
          begin
            {$IFDEF FPC}
            result := integer(PPointer(Data^.tString^)^);
            {$ELSE}
            Parameter := pointer(integer(AStackPtr) + SizeOf(Pointer) * 0 );
            PbtUTF8String(Parameter^)^ := PbtUTF8String(Data^.tString^)^;
            {$ENDIF}
          end;   
      btWideString :
          begin
            {$IFDEF FPC}
            result := integer(PPointer(Data^.tString^)^);
            {$ELSE}
            Parameter := Pointer(integer(AStackPtr) + SizeOf(Pointer) * 0);
            PbtWideString(Parameter^)^ := PbtWideString(Data^.tString^)^;
            {$ENDIF}
          end;
      btPChar :
          begin    
            {$IFDEF FPC}
            result := integer(PPointer(Data^.tString^)^);
            {$ELSE}
            Parameter := Pointer(integer(AStackPtr) + SizeOf(Pointer) * 0);
            PbtPChar(Parameter^)^ := PbtPChar(Data^.tString^)^;
            {$ENDIF}
          end;
      {
      btS64        : result := Data^.tS64^;
      }
      btRecord,
      btArray,
      btPointer,
      btObject     : result := cardinal(Data^.tPointer^);
      end;
      RunTime.Stack.Pop;
    end;
  end;

var iRes: integer;
begin
  CallInfo      := MethodPtr;
  RunTime       := CallInfo^.RunTime;
  ClassPtr      := CallInfo^.ClassData;
  ScriptMethod  := CallInfo^.MethodInfo;
  result := ParamsInStack(ScriptMethod) * SizeOf(Pointer);

  if not RunTime.Initialized then
     RunTime.Initialize;

  OldCodePos := RunTime.CodePos;
  try
    PushParamsToStack;

    if (ScriptMethod.DynIndex > -1) and (ClassPtr <> nil) then
    begin
      Methods  := GetClassMethods(ClassPtr);
      Index    := Methods[ScriptMethod.DynIndex];
      if Index > 0 then
      begin
        RunTime.CodePos := index;
        RunTime.Process;
      end;
    end else
    begin
      RunTime.CodePos   := ScriptMethod.CodePos;
      RunTime.Process;
    end;
  finally
    iRes := PopParamsFromStack;
    RunTime.CodePos := OldCodePos;
  end;
  PInteger(@_EDX)^ := iRes;
end;
{$Warnings on}

end.

