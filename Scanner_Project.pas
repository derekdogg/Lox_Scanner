unit Scanner_Project;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type

  TForm1 = class(TForm)
    BtnScan: TButton;
    memEdit: TMemo;
    chkRun: TCheckBox;
    chkEmit: TCheckBox;
    btnClear: TButton;
    MemRun: TMemo;
    MemCodes: TMemo;
    MemLocals: TMemo;
    procedure BtnScanClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
    prevbuffer,buffer : pointer;
    index : pInteger;
    size :  integer;
  public
    { Public declarations }
  end;



var
  Form1: TForm1;

implementation

uses
  typinfo,
  loxtypes,
  charIterator,
  LineIterator,
  scanner,

  TokenArray,
  compiler,
  vm,
  Table,
  Locals,
  values;

{$R *.dfm}



procedure TForm1.btnClearClick(Sender: TObject);
begin
  memEdit.Lines.clear;
end;


function sum(var a : integer) : integer;
begin
  a := a + 1;

  if (a > 20) then
  begin
    result :=  a;
    exit;
  end;

  result := sum(a) + sum(a) +  10;
end;



procedure TForm1.BtnScanClick(Sender: TObject);
var
  Scanner : TScanner;
  cc : TCompilerController;
  Tokens : TTokenIterator;
  VM :  TVirtualMachine;
  LoxFunction : pLoxFunction;
  i : integer;

begin
  //instructionPointer.Init(LoxFunction);
  MemRun.Lines.clear;
  MemCodes.Lines.clear;
  MemLocals.Lines.clear;

  try

   Scanner.Init(MemEdit.Lines.Text);
   Scanner.Scan;
   Tokens.Init(Scanner.Tokens);

   cc := TCompilerController.Create(Tokens,Scanner,TYPE_SCRIPT);
   try
     LoxFunction := cc.DoCompile;

     cc.LocalsToString(MemLocals.Lines);

     for i := 0 to cc.Count-1 do
     begin
        cc[i].ToString(MemRun.lines);
     end;

     VM.Init(LoxFunction,MemRun.Lines,nil{ MemCodes.Lines});
     VM.Run;

   finally
     DisposeFunction(LoxFunction);
     cc.free;
     vm.Finalize;
   end;

  finally
    Scanner.finalize;

  end;

end;


end.
