unit Scanner_Project;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type

  TForm1 = class(TForm)
    BtnScan: TButton;
    memEdit: TMemo;
    memRun: TMemo;
    chkRun: TCheckBox;
    chkEmit: TCheckBox;
    btnClear: TButton;
    procedure BtnScanClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  IntegerArray,
  ValueArray,
  Stacks,
  chunk,
  TokenArray,
  compiler,
  vm,
  ByteCodesArray,
  Table,
  ValueList,
  Locals,
  CompilerRules;

{$R *.dfm}



procedure TForm1.btnClearClick(Sender: TObject);
begin
  memEdit.Lines.clear;
end;

procedure TForm1.BtnScanClick(Sender: TObject);
var
  Scanner : TScanner;
  i,j : integer;
  Token : pToken;
  p : pChar;
  text : string;
  ln : String;
  Compiler : TCompiler;
  Tokens : TTokenIterator;
  InstructionPointer :  TInstructionPointer;
  constantIndex : byte;
  value : pValue; //represents a constant
  ByteCode : string;
  list : TList;
  VM :  TVirtualMachine;
  CompilerRules : TCompilerRules;
begin
   memRun.Lines.clear;




   Scanner.Init(MemEdit.Lines.Text);
   Scanner.Scan;
   Compiler := TCompiler.Create(Scanner,TYPE_SCRIPT);
   CompilerRules := TCompilerRules.Create;
   try


   if not chkRun.Checked then exit;

     CompilerRules.CreateRules(Compiler);

     Compiler.DoCompile;
     instructionPointer.Init(Compiler.Chunks);
     VM.Init(InstructionPointer,MemRun.Lines);
     VM.Run;
     // = INTERPRET_OK then
     //  MemRun.Lines.Add('result := ' + VM.Result.Value.ToString);
   finally
     VM.Finalize;
     CompilerRules.Free;
     Compiler.Free;
     Scanner.finalize;
   end;

end;


procedure TForm1.Button1Click(Sender: TObject);
var
  Locals : TLocalList;
  i : integer;
  a,b,c,d  : pLocal;
begin
  Locals.Init(true);

  new(a);
  new(b);
  new(c);
  Locals.Add(a);
  Locals.Add(b);
  Locals.Add(c);


  for i := locals.Count-1 downto 0 do
  begin
    d := locals[i];
  end;



  Locals.Finalize;
end;

end.
