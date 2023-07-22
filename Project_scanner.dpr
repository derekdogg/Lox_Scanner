program Project_scanner;

uses
  Forms,
  Scanner_Project in 'Scanner_Project.pas' {Form1},
  Chunk in 'Chunk.pas',
  LoxTypes in 'LoxTypes.pas',
  ByteArray in 'ByteArray.pas',
  ValueArray in 'ValueArray.pas',
  Stacks in 'Stacks.pas',
  Exceptions in 'Exceptions.pas',
  vm in 'vm.pas',
  TokenArray in 'TokenArray.pas',
  compiler in 'compiler.pas',
  ByteCodesArray in 'ByteCodesArray.pas',
  table in 'table.pas',
  ValueList in 'ValueList.pas',
  locals in 'locals.pas';

{$R *.res}

begin
  reportmemoryleaksonshutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
