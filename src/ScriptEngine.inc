{

ScriptEngine II: Options

You can set up the options for the ScriptEngine II here.

}

{--------------------------------Configuration---------------------------------}

{Enable this option to suppress the generation of debug info for the
 ScriptEngine II files. This will prevent the integrated debugger from stepping
 into the ScriptEngine II Code.}
{$define SEII_NoDebugInfo}    

{This option will disable Range-Checking for the ScriptEngine II - Source. This
 allows faster execution and smaller program sizes. If you do not define this
 value, the default handling of range-checking will be used.} 
{$define SEII_DisableRangeCheck}

{The runtime has an event, which is raised before every opcode is executed. This
 is very usefull to keep the program alive while executing a script (e.g. by
 calling "Application.ProcessMessage" in the event). If you do not want to use
 this event, you can set this define. This will save a few milliseconds of
 CPU-Time.} 
{.$define SEII_NoOpCodeEvent}

{This define will enable the caching for stack data during the execution of the
 script. If you enable this define, the runtime will execute faster and causes
 less memory fragmentation. But the memory usage will increase for about
 32 KBytes per RunTime-Instance. You should only disable this define under 
 special circumstances.} 
{$define SEII_UseRunTimeCache}

{This define will make the runtime cache bigger. The cache has a default size of
 32 KB, with this define it has 1.5 MB. Note that this size is needed for every
 instance of TSE2RunTime. The bigger cache has the advantage, that more stack 
 items fit into the cache and that small classes and records are stored inside 
 the cache. This define will be ignored if SEII_UseRunTimeCache is disabled.}
{.$define SEII_BigRunTimeCache}

{If you enable this define, every class of the ScriptEngine II will have
 TPersistant as the root object, otherwise TObject is used.} 
{.$define SEII_UsePersistantAsRoot}

{If you disable this define, only simple usage detection of methods is used. 
 This improves the compilation speed, but increases the final size of the 
 compiled data. It is not advised to disable this feature.}
{$define SEII_UseSmartLinker}

{--------------------------------Include-Files---------------------------------}

{Enable this option to include extended math functions into the ScriptEngine II.
 This option will be ignored for FreePascal-Uses because FPC does not have the
 used routines yet.} 
{$define SEII_NoExtendedMath}

{Enable this option to include an included performance counter to the 
 ScriptEngine II. This will allow you to use the object "TTimeCounter" in the
 ScriptEngine. This object gives you the number of seconds between
 "TTimeCounter.Start" and "TTimeCounter.Stop". Linux-Uses does not have the
 used PerformanceCounter-Method, so this option is ignored in Linux.} 
{.$define SEII_IncludePerfTimer}

{If you enable this define, the "SoundEx*" - functions will be included into
 the ScriptEngine II - Strings-class. If you do not want to use any of these
 function, it is advised to disable this define. Smaller exe-files will be the
 result.} 
{.$define SEII_IncludeSoundEx}

{-----------------------------------Defines------------------------------------}

{In this section, the previous configuration will be applied. It is not advised
 to modify this section.}    

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF} 

{$IFDEF SEII_NoDebugInfo}
  {$D-}
{$ENDIF}

{$IFDEF SEII_DisableRangeCheck}
  {$R-}
{$ENDIF}     

{$IFDEF SEII_NoOpCodeEvent}
  {$DEFINE SEII_NO_RUN_EVENT}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SEII_NO_EXT_MATH}
{$ELSE}
  {$IFDEF SEII_NoExtendedMath}
    {$DEFINE SEII_NO_EXT_MATH}
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  {$IFDEF SEII_IncludePerfTimer}
    {$DEFINE SEII_INCLUDE_PERF_TIMER}
  {$ENDIF}
{$ENDIF}

{$IFDEF SEII_IncludeSoundEx}
  {$DEFINE SEII_USE_SOUND_EX}
{$ENDIF}

{$IFDEF SEII_UseRunTimeCache}
  {$DEFINE SEII_STACK_USE_CACHE}
  {$DEFINE SEII_MM_USE_CACHE}
  
  {$IFDEF SEII_BigRunTimeCache}
    {$DEFINE SEII_MM_CACHE_BIG}
  {$ENDIF}  
{$ENDIF}

{$IFDEF SEII_UsePersistantAsRoot}
  {$DEFINE SEII_INCLUDE_RTTI}
{$ENDIF}

{$IFDEF SEII_UseSmartLinker}
  {$DEFINE SEII_SMART_LINKING}
{$ENDIF}

{--------------------------------Compiler Selection----------------------------}

{ defines for Delphi 1.0 }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI_16BIT}
  {$DEFINE WIN16}
  {$DEFINE 16BIT}
{$ENDIF}

{ defines for Delphi 2.0 }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
{$ENDIF}

{ defines for C++Builder 1.0 }
{$IFDEF VER93}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER1}
  {$DEFINE CBUILDER1UP}
{$ENDIF}

{ defines for Delphi 3.0 }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
{$ENDIF}

{ defines for C++Builder 3.0 }
{$IFDEF VER110}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER3}
  {$DEFINE CBUILDER1UP}
  {$DEFINE CBUILDER3UP}
{$ENDIF}

{ defines for Delphi 4.0 }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
{$ENDIF}

{ defines for C++Builder 4.0 }
{$IFDEF VER125}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER4}
  {$DEFINE CBUILDER1UP}
  {$DEFINE CBUILDER3UP}
  {$DEFINE CBUILDER4UP}
{$ENDIF}

{ defines for Delphi 5.0 }
{$IFDEF VER130}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
{$ENDIF}

{ defines for C++Builder 5.0 }
{$IFDEF VER135}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER5}
  {$DEFINE CBUILDER1UP}
  {$DEFINE CBUILDER3UP}
  {$DEFINE CBUILDER4UP}
  {$DEFINE CBUILDER5UP}
{$ENDIF}

{ defines for Delphi 6.0 }
{$IFDEF VER140}
  {$DEFINE VER140UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
{$ENDIF}

{ defines for Delphi 7.0 }
{$IFDEF VER150}
  {$DEFINE VER140UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
{$ENDIF}

{ defines for Delphi 2005 }
{$IFDEF VER170}
  {$DEFINE VER140UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI2005UP}
  {$DEFINE BDS}
  {$DEFINE BDS3}
  {$DEFINE BDS3UP}
{$ENDIF}

{ defines for Delphi 2006 }
{$IFDEF VER180}
  {$DEFINE VER140UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI10A}  
  {$DEFINE DELPHI10UP}
  {$DEFINE DELPHI2005UP}
  {$DEFINE DELPHI2006UP}
  {$DEFINE BDS}
  {$DEFINE BDS4}
  {$DEFINE BDS3UP}
  {$DEFINE BDS4UP}
{$ENDIF}

{ defines for Delphi 2007 }
{$IFDEF VER185}
  {$UNDEF DELPHI10A}  // declared in VER180
  {$UNDEF DELPHI2006} // declared in VER180
  {$UNDEF BDS4}       // declared in VER180

  {$DEFINE DELPHI10B}    
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI2007UP}    
  {$DEFINE BDS5}
  {$DEFINE BDS5UP}
{$ENDIF}

{ defines for Delphi 2009 }
{$IFDEF VER200}
  {$DEFINE VER140UP}  
  {$DEFINE VER150UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI10A}  
  {$DEFINE DELPHI10UP}
  {$DEFINE DELPHI2005UP}
  {$DEFINE DELPHI2006UP}  
  {$DEFINE DELPHI2007UP}  
  {$DEFINE DELPHI2009UP}
  {$DEFINE BDS}
  {$DEFINE BDS4}
  {$DEFINE BDS3UP}
  {$DEFINE BDS4UP}
{$ENDIF}

{ defines for Delphi 2010 }
{$IFDEF VER210}
  {$DEFINE VER140UP}  
  {$DEFINE VER150UP}  
  {$DEFINE VER160UP}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI10} 
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI10A}  
  {$DEFINE DELPHI10UP}
  {$DEFINE DELPHI2005UP}
  {$DEFINE DELPHI2006UP}  
  {$DEFINE DELPHI2007UP}  
  {$DEFINE DELPHI2009UP}   
  {$DEFINE DELPHI2010UP}
  {$DEFINE BDS}
  {$DEFINE BDS4}
  {$DEFINE BDS3UP}
  {$DEFINE BDS4UP}
{$ENDIF}


{$IFDEF WIN32}
  {$DEFINE MSWINDOWS} //not automatically defined for Delphi 2 thru 5
  {$DEFINE 32BIT}
{$ENDIF}

{--------------------------------Compiler Selection----------------------------}
  
{$IFDEF DELPHI7UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}