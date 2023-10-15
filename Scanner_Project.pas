unit Scanner_Project;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type

  TForm1 = class(TForm)
    BtnScan: TButton;
    memEdit: TMemo;
    MemTokens: TMemo;
    memRun: TMemo;
    chkRun: TCheckBox;
    chkEmit: TCheckBox;
    MemLogging: TMemo;
    MemVmLog: TMemo;
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
  try
  MemTokens.lines.clear;

  Scanner.Init(MemEdit.Lines.Text);
  Scanner.Scan;
  MemTokens.Lines.BeginUpdate;
  for i := 0 to Scanner.TokenCount-1  do
  begin
     token := Scanner.Tokens.getItem(i);

     text := Scanner.ln.items[Token.Line].text;
     text := copy(text,token.Start,token.length);
     MemTokens.Lines.Add(format('------- TOKENS INFO %d -------------',[i]));
     MemTokens.Lines.Add(cTab + 'Token item size : ' + inttostr(Scanner.tokens.ItemSize));
     MemTokens.Lines.Add(cTab + 'Tokens Resize Count : ' + inttostr(Scanner.tokens.resizecount));
     MemTokens.Lines.Add(cTab + 'Tokens Capacity : ' + inttostr(Scanner.tokens.Capacity));
     MemTokens.Lines.Add(cTab + 'Tokens Kb : ' + floattostr(Scanner.tokens.Capacity/1000));
     MemTokens.Lines.Add(cTab + 'Tokens Slot Count : ' + inttostr(Scanner.tokens.SlotCount));


     MemTokens.Lines.Add(format('------- TOKEN INFO %d -------------',[i]));

     MemTokens.Lines.Add(cTab + 'Token Kind : ' + TTokenName[token.kind] + ' is equal to ' + text);
     MemTokens.Lines.Add(cTab + 'Line : ' + inttostr(Token.Line));
     MemTokens.Lines.Add(cTab + 'Length : ' + inttostr(Token.Length));
     MemTokens.Lines.Add(cTab + 'Starts at : ' + inttostr(Token.Start));

     MemTokens.Lines.Add(cTab + 'Ends   at : ' + inttostr(Token.Start + Token.Length-1));
     MemTokens.lines.Add(cCr);
  end;
  MemTokens.Lines.EndUpdate;



  //try compiler to see what happens;
   MemRun.Lines.Clear;

   
   if not chkRun.Checked then exit;

   MemRun.lines.add('-------------------------------');
   
   Compiler := TCompiler.Create(Scanner,MemLogging.Lines,TYPE_SCRIPT);
   CompilerRules := TCompilerRules.Create;
   CompilerRules.CreateRules(Compiler);
   try
     Compiler.DoCompile;
     instructionPointer.Init(Compiler.Chunks);
     VM.Init(InstructionPointer,MemRun.Lines,MemVmLog.Lines);
     VM.Run;
     // = INTERPRET_OK then
     //  MemRun.Lines.Add('result := ' + VM.Result.Value.ToString);
   finally
      VM.Finalize;
     CompilerRules.Free;
     Compiler.Free;
   end;



  finally
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
