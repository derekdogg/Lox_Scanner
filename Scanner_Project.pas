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
  i,j : integer;
  Token : TToken;
  p : pChar;
  text : string;
  ln : String;
  Compiler : TCompilerController;
  Tokens : TTokenIterator;
  InstructionPointer :  TInstructionPointer;
  constantIndex : byte;
  value : pValue; //represents a constant
  ByteCode : string;
  list : TList;
  VM :  TVirtualMachine;


  LoxFunction : pLoxFunction;

  globals : TValueList;
  constants : TValueList;
  

begin
  i := 0;
 // showmessage(inttostr(sum(i)));

  try
  //MemTokens.lines.clear;

  Scanner.Init(MemEdit.Lines.Text);
  Scanner.Scan;
  //MemTokens.Lines.BeginUpdate;
  for i := 0 to Scanner.TokenCount-1  do
  begin
     token := Scanner.Tokens[i];

     text := Scanner.ln.items[Token.Line].text;
     text := copy(text,token.Start,token.length);
     (*MemTokens.Lines.Add(format('------- TOKENS INFO %d -------------',[i]));
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
     MemTokens.lines.Add(cCr); *)
  end;

//  MemTokens.Lines.EndUpdate;



  //try compiler to see what happens;
 //  MemRun.Lines.Clear;


   if not chkRun.Checked then exit;

//   MemRun.lines.add('-------------------------------');

   Tokens.Init(Scanner.Tokens);


   globals := TValueList.create(true);
   constants := TValueList.create(true);
   Compiler := TCompilerController.Create(globals,Tokens,Scanner,TYPE_SCRIPT);
   try
     LoxFunction := Compiler.DoCompile;


     //instructionPointer.Init(LoxFunction);
     MemRun.Lines.clear;
     MemCodes.Lines.clear;
     MemLocals.Lines.clear;

     Compiler.LocalsToString(MemLocals.Lines);

     for i := 0 to Compiler.Count-1 do
     begin
        Compiler[i].ToString(MemRun.lines);

     end;

     VM.Init(LoxFunction,MemRun.Lines, MemCodes.Lines);
     VM.Run;
     // = INTERPRET_OK then
     //  MemRun.Lines.Add('result := ' + VM.Result.Value.ToString);


   finally
     Compiler.free;

   end;



  finally
    Scanner.finalize;

  end;

end;


end.
