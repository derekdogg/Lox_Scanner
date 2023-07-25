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
    BtnHash: TButton;
    Button1: TButton;
    chkRun: TCheckBox;
    chkEmit: TCheckBox;
    MemLogging: TMemo;
    MemVmLog: TMemo;
    procedure BtnScanClick(Sender: TObject);
    procedure BtnHashClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  Locals;

{$R *.dfm}



procedure TForm1.BtnHashClick(Sender: TObject);
var
  Entries : TValuePairs;

  value0 : pNameValue;
  value1 : pNameValue;
  value2 : pNameValue;

  s     : string;
  idx   : integer;


  //strings :  TloxStringIterator;
begin
  value1 := nil;
  Value0 := nil;
   

  Entries.Init;
  Entries.Add(Value0);


  value1 := Entries.Find('a');



  Entries.Finalize;

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

   if chkEmit.checked then
   begin
     exit; //needs fixing up with oplocals
   i := 0;
   Compiler := TCompiler.Create(Scanner,MemLogging.Lines);
   try
     //Compiler.expression; //???
     Compiler.DoCompile;
     InstructionPointer.Init(Compiler.Chunks);
     MemRun.Lines.add('Bytes : ' + inttostr(InstructionPointer.ByteCount));
     MemRun.Lines.add('Constants : ' + inttostr(InstructionPointer.ConstantCount));

     While InstructionPointer.Next <> nil do
     begin
        ByteCode := TOP_Code_name[TOpCodes(InstructionPointer.Current^)];

        if (InstructionPointer.Current^ = byte(OP_CONSTANT)) then
        begin
          if InstructionPointer.Next <> nil then
          begin
            constantIndex := InstructionPointer.Current^;
            value := InstructionPointer.Value(constantIndex);
            ByteCode := ByteCode + '=' + value.ToString;
          end;
        end;

        if (instructionPointer.Current^ = byte(OP_DEFINE_GLOBAL)) then
        begin
          if InstructionPointer.Next <> nil then
          begin
            constantIndex := InstructionPointer.Current^;
            value := InstructionPointer.Value(constantIndex);
            ByteCode := ByteCode + '=' + value.ToString;
          end;
        end;

        if (instructionPointer.Current^ = byte(OP_GET_GLOBAL)) then
        begin
          //this should be the index of the 
          if InstructionPointer.Next <> nil then
          begin
            constantIndex := InstructionPointer.Current^;
            value := InstructionPointer.Value(constantIndex);
            ByteCode := ByteCode + '=' + value.ToString;
          end;
        end;



        MemRun.Lines.Add(inttostr(i) + ':' + ByteCode);
        inc(i);
     end;
   finally
     Compiler.Free;
   end;
   end;
   if not chkRun.Checked then exit;

   MemRun.lines.add('-------------------------------');
   //spin up compiler again, to show result. At the moment only handles add, plus,divde, multiply. which is why (I think) (5 - (3 - 1)) + -1
   // is giving 4 currently - however, byte code is legit.
   Compiler := TCompiler.Create(Scanner,MemLogging.Lines);
   try
     Compiler.DoCompile;
     instructionPointer.Init(Compiler.Chunks);
     VM.Init(InstructionPointer,MemRun.Lines,MemVmLog.Lines);
     VM.Run;
     // = INTERPRET_OK then
     //  MemRun.Lines.Add('result := ' + VM.Result.Value.ToString);
   finally
      VM.Finalize;
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
