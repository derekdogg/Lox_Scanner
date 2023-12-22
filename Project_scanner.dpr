program Project_scanner;

uses
  ExceptionLog,
  Forms,
  Scanner_Project in 'Scanner_Project.pas' {fmScript},
  LoxTypes in 'LoxTypes.pas',
  Exceptions in 'Exceptions.pas',
  NewVm in 'NewVm.pas',
  TokenArray in 'TokenArray.pas',
  compiler in 'compiler.pas',
  locals in 'locals.pas',
  Addition in 'Addition.pas',
  scanner in 'scanner.pas',
  values in 'values.pas',
  Natives in 'Natives.pas',
  subtraction in 'subtraction.pas',
  ValueManager in 'ValueManager.pas',
  HashTable in 'HashTable.pas';

{$R *.res}

begin
  reportmemoryleaksonshutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmScript, fmScript);
  Application.Run;
end.
