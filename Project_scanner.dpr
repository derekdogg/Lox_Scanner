program Project_scanner;

uses
  Forms,
  Scanner_Project in 'Scanner_Project.pas' {Form1},
  Chunk in 'Chunk.pas',
  LoxTypes in 'LoxTypes.pas',
  ByteArray in 'ByteArray.pas',
  DoubleArray in 'DoubleArray.pas',
  Stacks in 'Stacks.pas',
  Exceptions in 'Exceptions.pas';

{$R *.res}

begin
  reportmemoryleaksonshutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.