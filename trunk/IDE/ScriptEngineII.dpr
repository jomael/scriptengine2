program ScriptEngineII;

{%File 'ScriptEngine.inc'}

uses
  FastMM4,
  Controls,
  Windows,
  Forms,
  main in 'main.pas' {MainForm},
  uFormCodeEditor in 'units\Editor\uFormCodeEditor.pas',
  uFormCompileMessages in 'units\Editor\uFormCompileMessages.pas',
  uScriptProject in 'units\ScriptProject\uScriptProject.pas',
  ConsoleForm in 'units\Console\ConsoleForm.pas' {ConsoleWindow},
  Console in 'units\Console\CONSOLE.PAS',
  uFrmAbout in 'units\Forms\uFrmAbout.pas' {frmAbout},
  SynHighlighterSE2 in 'units\Editor\SynHighlighterSE2.pas',
  uFrmSearch in 'units\Forms\uFrmSearch.pas' {SearchDialog},
  uPackageLoader in 'units\uPackageLoader.pas',
  uPackageInfo in 'units\IDE Controls\uPackageInfo.pas',
  ScriptTestUnits in 'units\ScriptTests\ScriptTestUnits.pas';

{$R *.res}

begin                     
  Screen.Cursors[crHandPoint] := LoadCursor(0, IDC_HAND);
  Screen.Cursors[crSizeNS]    := LoadCursor(0, IDC_SIZENS);
  Screen.Cursors[crSizeWE]    := LoadCursor(0, IDC_SIZEWE);
  Screen.Cursors[crNo]        := LoadCursor(0, IDC_NO);

  Application.Initialize;
  Application.Title := 'ScriptEngine II';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
