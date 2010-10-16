unit uSE2DebugData;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2OpCode, uSE2Consts, uSE2RunType, uSE2PEData;

type
  TSE2DebugHelper = class(TObject)
  private
    class function OperationToStr(const Op: byte): string;
    class function ComparisonToStr(const Op: byte): string;
    class function GetClassPtrName(RunTime: Pointer; Ptr: Pointer): string;
  public
    class function VarTypeToStr(AType: TSE2TypeIdent): string;
    class function VarContentToStr(Data: PSE2VarData; RunTime: Pointer; MaxStrLen: integer = -1): string;
    class function OpCodeToStr(PE: TSE2PE; OpCode: PSE2OpDefault): string; overload;
    class function OpCodeToStr(OpCode: TSE2OpCode): string; overload;
  end;

implementation

uses
  StrUtils, uSE2RunTime, uSE2SystemUnit;

{ TSE2DebugHelper }



const
  TSE2OpCodeStr : array[TSE2OpCode] of string = (
                // Default
                'soNOOP',

                // Stack Methods
                'soSTACK_INC', 'soSTACK_INC_COUNT',
                'soSTACK_DEC', 'soSTACK_DEC_NODEL', 'soSTACK_DEC_COUNT',

                // Memory Methods
                'soMEM_MAKE', 'soMEM_REC_MAKE', 'soMEM_REC_FREE',

                // Program execution flow
                'soFLOW_GOTO', 'soFLOW_JIZ', 'soFLOW_JNZ', 'soFLOW_CALL', 'soFLOW_CALLEX', 'soFLOW_CALLDYN', 'soFLOW_CALLPTR',
                'soFLOW_RET', 'soFLOW_PUSHRET',

                // Aritmetic operation
                'soOP_OPERATION', 'soOP_COMPARE',
                'soOP_FASTCOMPARE', 'soOP_FASTOPERATION',

                // Data Movement
                'soDAT_COPY_TO', 'soDAT_COPY_FROM',
                'soDAT_MOVE_TO', 'soDAT_MOVE_FROM',
                'soDAT_CONVERT', 'soDAT_CHANGETYPE',
                
                // Data Pointer Movement
                'soDAT_PTR_LOAD', 'soDAT_PTR_SAVE',
                'soDAT_PTR_CREATE', 'soDAT_PTR_FREE',

                // Data Assign
                'soDAT_SetInt', 'soDAT_SetFloat', 'soDAT_SetPtr', 'soDAT_LOADRES', 'soDAT_CLEAR',
                                           
                // Special Data Assign
                'soDAT_PUSHInt32', 'soDAT_PUSHInt64', 'soDAT_PUSHUInt64', 'soDAT_PUSHFLOAT4', 'soDAT_PUSHFLOAT8',
                'soDAT_PUSHPtr', 'soDAT_PUSHRES',

                // Special Data
                'soSPEC_INCP', 'soSPEC_CREATE', 'soSPEC_DESTROY', 'soSPEC_UNREF', 'soSPEC_DECP',
                'soSPEC_GetRef', 'soSPEC_GetProcPtr',

                // Record Data
                'soREC_MAKE', 'soREC_FREE', 'soREC_COPY_TO', 'soREC_MARK_DEL', 'soREC_DEL_RECS',
                'soREC_EQUAL', 'soREC_UNEQUAL',

                // integer increment
                'soINT_INCSTATIC', 'soINT_INCSTACK',

                // integer decrement
                'soINT_DECSTATIC', 'soINT_DECSTACK',

                // Safe-Blocks
                'soSAFE_TRYFIN', 'soSAFE_TRYEX', 'soSAFE_BLOCK', 'soSAFE_TRYEND', 'soSAFE_SJUMP',
                'soSAFE_INTER', 'soSAFE_STACK', 'soSAFE_PEX',

                // special runtime data
                'soDEBUG_META', 'soFINIT_STACK', 'soSAVE_INSTANCE',

                // Meta
                'soMETA_PUSH', 'soMETA_SHARE', 'soMETA_CNAME', 'soMETA_CAST'
                );

class function TSE2DebugHelper.OpCodeToStr(PE: TSE2PE; OpCode: PSE2OpDefault): string;
begin
  if OpCode = nil then
  begin
    result := '[nil]';
    exit;
  end;
  case OpCode.OpCode of
  soNOOP                 : result := 'NOOP';
  soSTACK_INC            : result := Format('PUSH [%s]', [VarTypeToStr(PSE2OpSTACK_INC(OpCode).AType)]);
  soSTACK_INC_COUNT      : result := Format('PUSH [%d %s]', [PSE2OpSTACK_INC_COUNT(OpCode).Count, VarTypeToStr(PSE2OpSTACK_INC_COUNT(OpCode).AType)]);
  soSTACK_DEC            : result := 'POP';
  soSTACK_DEC_NODEL      : result := 'POP [no delete]';
  soSTACK_DEC_COUNT      : result := Format('POP [%d]', [PSE2OpSTACK_DEC_COUNT(OpCode).Count]);



  soFLOW_GOTO            : result := Format('GOTO [%d]', [PSE2OpFLOW_GOTO(OpCode).Position]);
  soFLOW_JIZ             : result := Format('JIZ [%d]', [PSE2OpFLOW_JIZ(OpCode).Position]);
  soFLOW_JNZ             : result := Format('JNZ [%d]', [PSE2OpFLOW_JNZ(OpCode).Position]);
  soFLOW_CALL            : result := Format('CALL [%d]', [PSE2OpFLOW_CALL(OpCode).Position]);
  soFLOW_CALLEX          : result := Format('CALL EX [%s.%s]', [PE.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex].AUnitName, PE.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex].Name]);
  soFLOW_CALLDYN         : result := Format('CALL DYN [%d]', [PSE2OpFLOW_CALLDYN(OpCode).Offset]);
  soFLOW_CALLPTR         : result := 'CALL PTR';

  soFLOW_RET             : result := 'RET';
  soFLOW_PUSHRET         : result := Format('PUSH RET [%d]', [PSE2OpFLOW_PUSHRET(OpCode).Position]);
  soOP_OPERATION         : result := Format('OP [%s]', [OperationToStr(PSE2OpOP_OPERATION(OpCode).OpType)]);
  soOP_FASTOPERATION     : result := Format('FOP [%s]', [OperationToStr(PSE2OpOP_FASTOPERATION(OpCode).OpType)]);
  soOP_COMPARE           : result := Format('CMP [%s]', [ComparisonToStr(PSE2OpOP_COMPARE(OpCode).CompType)]);
  soOP_FASTCOMPARE       : result := Format('FCMP [%s]', [ComparisonToStr(PSE2OpOP_FASTCOMPARE(OpCode).CompType)]);
  soDAT_COPY_TO          : result := Format('POP TO [%d %s]', [PSE2OpDAT_COPY_TO(OpCode).Target, IfThen(PSE2OpDAT_COPY_TO(OpCode).Static, 'Static', 'Dynamic')]);
  soDAT_COPY_FROM        : result := Format('PUSH FROM [%d %s]', [PSE2OpDAT_COPY_FROM(OpCode).Source, IfThen(PSE2OpDAT_COPY_FROM(OpCode).Static, 'Static', 'Dynamic')]);
  soDAT_MOVE_TO          : result := Format('MOVE TO [%d %s]', [PSE2OpDAT_MOVE_TO(OpCode).Target, IfThen(PSE2OpDAT_MOVE_TO(OpCode).Static, 'Static', 'Dynamic')]);
  soDAT_MOVE_FROM        : result := Format('MOVE FROM [%d %s]', [PSE2OpDAT_MOVE_FROM(OpCode).Source, IfThen(PSE2OpDAT_MOVE_FROM(OpCode).Static, 'Static', 'Dynamic')]);
  soDAT_CONVERT          : result := Format('CHANGE [%s %d]', [VarTypeToStr(PSE2OpDAT_CONVERT(OpCode).NewType), PSE2OpDAT_CONVERT(OpCode).Index]);
  soDAT_SetInt           : result := Format('MAKE INT [%d]', [PSE2OpDAT_SetInt(OpCode).Value]);
  soDAT_SetFloat         : result := Format('MAKE FLT [%n]', [PSE2OpDAT_SetFloat(OpCode).Value]);
  soDAT_SetPtr           : result := Format('MAKE PTR [%x]', [PtrInt(PSE2OpDAT_SetPtr(OpCode).Value)]);
  soDAT_LOADRES          : result := Format('MAKE STR [%s]', [PE.Strings[PSE2OpDAT_LOADRES(OpCode).Index]]);
  soDAT_CLEAR            : result := 'CLEAR';


  soDAT_PUSHInt32        : result := Format('PUSH Int32 [%d]', [PSE2OpDAT_PUSHInt32(OpCode).Value]);   
  soDAT_PUSHInt64        : result := Format('PUSH Int64 [%d]', [PSE2OpDAT_PUSHInt64(OpCode).Value]);  
  soDAT_PUSHUInt64       : result := Format('PUSH UInt64 [%u]', [PSE2OpDAT_PUSHUInt64(OpCode).Value]);  
  soDAT_PUSHFloat4       : result := Format('PUSH single [%n]', [PSE2OpDAT_PUSHFloat4(OpCode).Value]); 
  soDAT_PUSHFloat8       : result := Format('PUSH double [%n]', [PSE2OpDAT_PUSHFloat8(OpCode).Value]);
  soDAT_PUSHPtr          : result := Format('PUSH ptr [%x]', [PtrInt(PSE2OpDAT_PUSHPtr(OpCode).Value)]);
  soDAT_PUSHRES          : result := Format('PUSH str [%s]', [PE.Strings[PSE2OpDAT_PUSHRES(OpCode).Index]]);

  soSPEC_INCP            : result := Format('INCP [%x %s]', [PSE2OpSPEC_INCP(OpCode).Offset, VarTypeToStr(PSE2OpSPEC_INCP(OpCode).newType)]);
  soSPEC_CREATE          : result := Format('NEW [%d %s.%s]', [PSE2OpSPEC_CREATE(OpCode).Variables, PE.MetaData[PSE2OpSPEC_CREATE(OpCode).MetaIndex].AUnitName, PE.MetaData[PSE2OpSPEC_CREATE(OpCode).MetaIndex].Name]);
  soSPEC_DESTROY         : result := 'DELETE';
  soSPEC_UNREF           : result := 'UNREF';   
  soSPEC_DECP            : result := Format('DECP [%d]', [PSE2OpSPEC_DECP(OpCode).Target]);
  soSPEC_GetProcPtr      : result := Format('PREF [%s, %s]', [ PE.MetaData[PSE2OpSPEC_GetProcPtr(OpCode).MetaIndex].Name, IfThen(PSE2OpSPEC_GetProcPtr(OpCode).HasSelf, 'Self', '-') ]);

  soREC_MAKE             : result := Format('REC.NEW [%d %s.%s]', [PSE2OpREC_MAKE(OpCode).Variables, PE.MetaData[PSE2OpREC_MAKE(OpCode).MetaIndex].AUnitName, PE.MetaData[PSE2OpREC_MAKE(OpCode).MetaIndex].Name]);
  soREC_FREE             : result := Format('REC.FREE [%d]', [PSE2OpREC_FREE(OpCode).Offset]);
  soREC_COPY_TO          : result := Format('REC.POP TO [%d %s]', [PSE2OpREC_COPY_TO(OpCode).Target, IfThen(PSE2OpREC_COPY_TO(OpCode).Target >= 0, 'Static', 'Dynamic')]);
  soREC_MARK_DEL         : result := 'REC.UNUSE';
  soREC_DEL_RECS         : result := Format('REC.GC [%d]', [PSE2OpREC_DEL_RECS(OpCode).MaxRecords]);
  soREC_EQUAL            : result := 'REC.SAME';
  soREC_UNEQUAL          : result := 'REC.DIFF';

  soMEM_MAKE             : result := Format('MEM.MAKE [%s]', [VarTypeToStr(PSE2OpMEM_MAKE(OpCode).AType)]);
  soMEM_REC_MAKE         : result := Format('MEM.REC [%d %s.%s]', [PSE2OpMEM_REC_MAKE(OpCode).Variables, PE.MetaData[PSE2OpMEM_REC_MAKE(OpCode).MetaIndex].AUnitName, PE.MetaData[PSE2OpMEM_REC_MAKE(OpCode).MetaIndex].Name]);
  soMEM_REC_FREE         : result := Format('MEM.FREE [%d]', [PSE2OpMEM_REC_FREE(OpCode).Offset]);

  soFINIT_STACK          : result := 'FSTK';
  soDEBUG_META           : result := 'META';
  soMETA_CNAME           : result := 'CNM';
  soMETA_PUSH            : result := Format('MPUT [%d]', [PSE2OpMETA_PUSH(OpCode).MetaIndex]);
  soMETA_SHARE           : result := Format('MSHR [%d]', [PSE2OpMETA_SHARE(OpCode).MetaIndex]);
  soMETA_CAST            : result := Format('MCST [%d]', [PSE2OpMETA_CAST(OpCode).MetaIndex]);


  soINT_INCSTATIC        : result := Format('INC STATIC [%d %d]', [PSE2OpINT_INCSTATIC(OpCode).Offset, PSE2OpINT_INCSTATIC(OpCode).Value]);
  soINT_INCSTACK         : result := Format('INC DYN [%d %d]', [PSE2OpINT_INCSTACK(OpCode).Offset, PSE2OpINT_INCSTACK(OpCode).Value]);
  soINT_DECSTATIC        : result := Format('DEC STATIC [%d %d]', [PSE2OpINT_DECSTATIC(OpCode).Offset, PSE2OpINT_DECSTATIC(OpCode).Value]);
  soINT_DECSTACK         : result := Format('DEC DYN [%d %d]', [PSE2OpINT_DECSTACK(OpCode).Offset, PSE2OpINT_DECSTACK(OpCode).Value]);

  soSAFE_TRYFIN          : result := Format('TRY FIN [%d %d]', [PSE2OpSAFE_TRYFIN(OpCode).SavePos, PSE2OpSAFE_TRYFIN(OpCode).LeavePos]);
  soSAFE_TRYEX           : result := Format('TRY EX [%d %d]', [PSE2OpSAFE_TRYEX(OpCode).SavePos, PSE2OpSAFE_TRYEX(OpCode).LeavePos]);
  soSAFE_BLOCK           : result := Format('TRY BLOCK [%d]', [PSE2OpSAFE_BLOCK(OpCode).SkipPoint]);
  soSAFE_TRYEND          : result := 'TRY END';
  soSAFE_SJUMP           : result := Format('TRY JUMP [%d %d]', [PSE2OpSAFE_SJUMP(OpCode).Target, PSE2OpSAFE_SJUMP(OpCode).ExitTo]);
  soSAFE_INTER           : result := 'ITRP';
  soSAFE_STACK           : result := 'ISCK';
  soSAFE_PEX             : result := 'PEX';

  soSAVE_INSTANCE        : result := 'INST';


  else                     result := '[unknown] {'+TSE2OpCodeStr[OpCode.OpCode]+'}';
  end;
end;


class function TSE2DebugHelper.ComparisonToStr(const Op: byte): string;
begin
  case Op of
  1 : result := '=';
  2 : result := '<';
  3 : result := '>';
  4 : result := '>=';
  5 : result := '<=';
  6 : result := '<>';
  else result := 'unknown ['+IntToStr(op)+']';
  end;
end;

class function TSE2DebugHelper.OpCodeToStr(OpCode: TSE2OpCode): string;
begin
  result := TSE2OpCodeStr[OpCode];
end;

class function TSE2DebugHelper.OperationToStr(const Op: byte): string;
begin
  case Op of
  1  : result := 'negate';
  20 : result := 'bit not';
  21 : result := 'bool not';
  2  : result := '+';
  3  : result := '-';
  4  : result := '*';
  5  : result := '/';
  6  : result := 'and';
  7  : result := 'or';
  8  : result := 'xor';
  9  : result := 'mod';
  10 : result := 'shr';
  11 : result := 'shl';
  else result := 'unknown ['+IntToStr(Op)+']';
  end;
end;

class function TSE2DebugHelper.GetClassPtrName(RunTime: Pointer; Ptr: Pointer): string;
var aRun : TSE2RunTime;
    meta : TSE2MetaEntry;
begin
  result := '';
  aRun := RunTime;
  if aRun.ClassList.IndexOf(Ptr) > -1 then
  begin
    meta := aRun.ExecutionData.PackedData.GetClassMeta(Ptr); // TSE2MetaEntry( Pointer(integer(Ptr) + uSE2SystemUnit.vmtScriptMetaEntry) );
    try
      //result := '0x' +IntToHex(Integer(Ptr), 8);
      result := '['+meta.AUnitName + '.' + meta.Name +']';
    except
      result := '[{Exception}]';
    end;
  end;
end;

class function TSE2DebugHelper.VarContentToStr(Data: PSE2VarData; RunTime: Pointer; MaxStrLen: integer = -1): string;
var s: string;
begin
  try
  case Data^.AType of
  btU8  : result := IntToStr(Data.tU8^);
  btS8  : result := IntToStr(Data.tS8^);
  btU16 : result := IntToStr(Data.tU16^);
  btS16 : result := IntToStr(Data.tS16^);
  btU32 : result := IntToStr(Data.tU32^);
  btS32 : result := IntToStr(Data.tS32^);
  btReturnAddress :
      result := IntToStr(Data.ts64^ and $FFFFFFFF);
  btS64 :
      result := IntToStr(Data.ts64^);
  btU64 :
      result := IntToStr(Data.ts64^);
  btRecord :
      begin
        if Pointer(Data^.tPointer^) = nil then
           result := 'nil'
        else
        begin
          result := '0x' + IntToHex(Integer(Data^.tPointer^), 8);
        end;
      end;
  btPointer, btObject :
      begin
        if Pointer(Data^.tPointer^) = nil then
           result := 'nil'
        else
        begin
           result := '0x' +IntToHex(Integer(Data^.tPointer^), 8);
           if RunTime <> nil then
              Result := result + ' '+GetClassPtrName(RunTime, PPointer(Data^.tPointer)^);

        end;
      end;
  btSingle :
      result := FloatToStrF(Data^.tSingle^, ffNumber, 8, 2);
  btDouble :
      result := FloatToStrF(Data^.tDouble^, ffNumber, 16, 8);
  btString,
  btWideString,
  btUTF8String,
  btAnsiString,
  btPAnsiChar,
  btPWideChar,
  btPChar :
      begin
        if Pointer(Data^.tPointer^) = nil then
           result := '{nil string}'
        else
        begin
           result := '''';

           case Data.AType of
           btString      : s := PbtString(Data^.tPointer^)^;
           btUTF8String  : s := Utf8ToAnsi(PbtUTF8String(Data^.tPointer^)^);
           btPChar       : s := PbtPChar(Data^.tPointer^)^;
           btWideString  : s := Utf8ToAnsi( UTF8Encode( PbtWideString(Data^.tPointer^)^ ) );
           btAnsiString  : s := string( AnsiString( PbtAnsiString(Data^.tPointer^)^ ));
           btPAnsiChar   : s := string( AnsiString(PbtPAnsiChar(Data^.tPointer^)^) );
           btPWideChar   : s := string( TbtWideString(PbtPWideChar(Data^.tPointer^)^) );
           end;

           if MaxStrLen > -1 then
             if MaxStrLen < length(s) then
              s := Copy(s, 1, MaxStrLen) + '...';

           result := result + s + '''';
        end;
      end;
  btProcPtr :
      begin
        if Pointer(Data^.tPointer) = nil then
           result := 'invalid'
        else
        begin
          if PPointer(Integer(Data^.tPointer) + SizeOf(Pointer))^ = nil then
             result := ''
          else
          begin
            result := ' 0x' +IntToHex(integer(PPointer(Integer(Data^.tPointer) + SizeOf(Pointer))^), 8);
            if RunTime <> nil then
               Result := result + ' '+GetClassPtrName(RunTime, PPointer(Integer(Data^.tPointer) + SizeOf(Pointer))^);
          end;

          if PPointer(Data^.tPointer)^ = nil then
             result := 'nil'
          else
             result := '0x' + IntToHex(Integer(Data^.tPointer^), 8) + result;
        end;
      end;
  end;
  if Data^.RefContent then
     result := '@['+result+']';
  except
    result := '{EXCEPTION}';
  end;
end;

class function TSE2DebugHelper.VarTypeToStr(AType: TSE2TypeIdent): string;
begin
  case aType of
  btU8                    : result := 'U8';
  btS8                    : result := 'S8';
  btU16                   : result := 'U16';
  btS16                   : result := 'S16';
  btReturnAddress         : result := 'RET';
  btU32                   : result := 'U32';
  btS32                   : result := 'S32';
  btS64                   : result := 'S64';
  btU64                   : result := 'U64';
  btSingle                : result := 'Float';
  btDouble                : result := 'Double';
  btString                : result := 'String';
  btPointer               : result := 'Pointer';
  btPChar                 : result := 'PChar';
  btObject                : result := 'object';
  btUTF8String            : result := 'UTF8String';
  btWideString            : result := 'WideString';
  btAnsiString            : result := 'AnsiString';
  btPAnsiChar             : result := 'PAnsiChar';
  btPWideChar             : result := 'PWideChar';
  btRecord                : result := 'record';
  btProcPtr               : result := 'ProcPtr';
  else                      result := '{unkown}';
  end;
end;

end.
