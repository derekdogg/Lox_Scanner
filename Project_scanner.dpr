program Project_scanner;

uses
  Forms,
  Scanner_Project in 'Scanner_Project.pas' {Form1},
  LoxTypes in 'LoxTypes.pas',
  Exceptions in 'Exceptions.pas',
  vm in 'vm.pas',
  TokenArray in 'TokenArray.pas',
  compiler in 'compiler.pas',
  table in 'table.pas',
  locals in 'locals.pas',
  Addition in 'Addition.pas',
  scanner in 'scanner.pas',
  Values in 'Values.pas',
  Natives in 'Natives.pas',
  subtraction in 'subtraction.pas';

{$R *.res}

begin
  reportmemoryleaksonshutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
