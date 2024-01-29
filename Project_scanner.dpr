program Project_scanner;

uses
  Forms,
  Scanner_Project in 'Scanner_Project.pas' {fmScript},
  LoxTypes in 'LoxTypes.pas',
  Exceptions in 'Exceptions.pas',
  NewVm in 'NewVm.pas',
  TokenArray in 'TokenArray.pas',
  compiler in 'compiler.pas',
  locals in 'locals.pas',
  values in 'values.pas',
  Natives in 'Natives.pas',
  ValueManager in 'ValueManager.pas',
  HashTable in 'HashTable.pas',
  Arrays in 'Arrays.pas',
  NewScanner in 'NewScanner.pas';

{$R *.res}

begin
  reportmemoryleaksonshutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmScript, fmScript);
  Application.Run;
end.
