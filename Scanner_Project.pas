unit Scanner_Project;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type

  TForm1 = class(TForm)
    BtnScan: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    BtnHash: TButton;
    Button1: TButton;
    chkRun: TCheckBox;
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
  ValueList;

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

  VM :  TVirtualMachine;
begin
  try
  Memo2.lines.clear;

  Scanner.Init(Memo1.Lines.Text);
  Scanner.Scan;
  Memo2.Lines.BeginUpdate;
  for i := 0 to Scanner.TokenCount-1  do
  begin
     token := Scanner.Tokens.getItem(i);

     text := Scanner.ln.items[Token.Line].text;
     text := copy(text,token.Start,token.length);
     Memo2.Lines.Add(format('------- TOKENS INFO %d -------------',[i]));
     Memo2.Lines.Add(cTab + 'Token item size : ' + inttostr(Scanner.tokens.ItemSize));
     Memo2.Lines.Add(cTab + 'Tokens Resize Count : ' + inttostr(Scanner.tokens.resizecount));
     Memo2.Lines.Add(cTab + 'Tokens Capacity : ' + inttostr(Scanner.tokens.Capacity));
     Memo2.Lines.Add(cTab + 'Tokens Kb : ' + floattostr(Scanner.tokens.Capacity/1000));
     Memo2.Lines.Add(cTab + 'Tokens Slot Count : ' + inttostr(Scanner.tokens.SlotCount));




     Memo2.Lines.Add(format('------- TOKEN INFO %d -------------',[i]));

     Memo2.Lines.Add(cTab + 'Token Kind : ' + TTokenName[token.kind] + ' is equal to ' + text);
     Memo2.Lines.Add(cTab + 'Line : ' + inttostr(Token.Line));
     Memo2.Lines.Add(cTab + 'Length : ' + inttostr(Token.Length));
     Memo2.Lines.Add(cTab + 'Starts at : ' + inttostr(Token.Start));

     Memo2.Lines.Add(cTab + 'Ends   at : ' + inttostr(Token.Start + Token.Length-1));
     Memo2.lines.Add(cCr);
  end;
  Memo2.Lines.EndUpdate;

  if not chkRun.Checked then exit;

  //try compiler to see what happens;
   Memo3.Lines.Clear;
   i := 0;
   Compiler := TCompiler.Create(Scanner);
   try
     //Compiler.expression; //???
     Compiler.DoCompile;
     InstructionPointer.Init(Compiler.Chunks);
     Memo3.Lines.add('Bytes : ' + inttostr(InstructionPointer.ByteCount));
     Memo3.Lines.add('Constants : ' + inttostr(InstructionPointer.ConstantCount));

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



        Memo3.Lines.Add(inttostr(i) + ':' + ByteCode);
        inc(i);
     end;
   finally
     Compiler.Free;
   end;
   
   Memo3.lines.add('-------------------------------');
   //spin up compiler again, to show result. At the moment only handles add, plus,divde, multiply. which is why (I think) (5 - (3 - 1)) + -1
   // is giving 4 currently - however, byte code is legit.
   Compiler := TCompiler.Create(Scanner);
   try
     Compiler.DoCompile;
     instructionPointer.Init(Compiler.Chunks);
     VM.Init(InstructionPointer,Memo3.Lines);
     VM.Run;
     // = INTERPRET_OK then
     //  Memo3.Lines.Add('result := ' + VM.Result.Value.ToString);
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
  values : TValueList;
  str : pValue;
  num : pValue;
  bool : pValue;
  ValueStack : TValueStack;
  pop : pValue;


  v : pNameValue;
begin
  new(v);






  ValueStack.init;
  values.Init(true);

  str := NewString('Fred');
  num := NewNumber(1.5);
  bool := NewBool(true);


  v.name := str;
  v.value := num;

  Values.Add(Str);
  Values.Add(Num);
  Values.Add(Bool);

  ValueStack.Push(str);
  ValueStack.Push(num);
  ValueStack.Push(bool);

  pop := ValueStack.pop;    Assert(pop <> nil);
  Memo3.Lines.Add(pop.ToString);

  pop := ValueStack.pop;    Assert(pop <> nil);
  Memo3.Lines.Add(pop.ToString);
  pop := ValueStack.pop;    Assert(pop <> nil);
  Memo3.Lines.Add(pop.ToString);



  ValueStack.Finalize;
  Values.Finalize;
  dispose(v);
end;

end.
