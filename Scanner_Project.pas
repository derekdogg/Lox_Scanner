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
    procedure BtnScanClick(Sender: TObject);
    procedure BtnHashClick(Sender: TObject);
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
  Table;

{$R *.dfm}



procedure TForm1.BtnHashClick(Sender: TObject);
var
  Entries : TLoxStrings;

  value1 : pLoxString;
  value2 : pLoxString;
  value3 : pLoxString;
  value4 : pLoxString;
  value5 : pLoxString;
  value6 : pLoxString;
  value7 : pLoxString;
  value8 : pLoxString;
  value9 : pLoxString;
  value10 : pLoxString;
  value11 : pLoxString;
  value12 : pLoxString;

  s     : string;
  idx   : integer;
  l     : TList;

  Items :  PLoxStringItems;

  target : pLoxString;

  strings :  TloxStringIterator;
begin
  Entries.Init;

  value1 := NewLoxString('abcde');
  value2 := NewLoxString('bedfg');
  value3 := NewLoxString('csdff');
  value4 := NewLoxString('dsssdf');
  value5 := NewLoxString('esffd');
  value6 := NewLoxString('fsdfsdf');
  value7 := NewLoxString('gsdfsdf');
  value8 := NewLoxString('hsdfsdf');
  value9 := NewLoxString('isdfsdf');
  value10 := NewLoxString('jsfdsdf');
  value11 := NewLoxString('ksdfsdf');
  value12 := NewLoxString('lsdfsdf');

  Entries.Add(value1);
  Entries.Add(value2);
  Entries.Add(value3);
  Entries.Add(value4);
  Entries.Add(value5);
  Entries.Add(value6);
  Entries.Add(value7);
  Entries.Add(value8);
  Entries.Add(value9);
  Entries.Add(value10);
  Entries.Add(value11);
  Entries.Add(value12);



  target := Entries.FindEntry(value1);

  target := Entries.FindEntry(value10);


  strings.init(Entries);

  (*while Strings.MoveNext <> nil do //note traditional iterator here won't work.
  begin
    memo3.Lines.add(Strings.Current.chars);
  end; *)


  Entries.Finalize; //this is just freeing the range of pointers, not the actual pointers themselves.
  dispose(value1);
  dispose(value2);
  dispose(value3);
  dispose(value4);
  dispose(value5);
  dispose(value6);
  dispose(value7);
  dispose(value8);
  dispose(value9);
  dispose(value10);
  dispose(value11);
  dispose(value12);

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

     Memo2.Lines.Add(cTab + TTokenName[token.kind] + '=' + text);
     Memo2.Lines.Add(cTab + 'Line : ' + inttostr(Token.Line));
     Memo2.Lines.Add(cTab + 'Length : ' + inttostr(Token.Length));
     Memo2.Lines.Add(cTab + 'Starts at : ' + inttostr(Token.Start));

     Memo2.Lines.Add(cTab + 'Ends   at : ' + inttostr(Token.Start + Token.Length-1));
     Memo2.lines.Add(cCr);
  end;
  Memo2.Lines.EndUpdate;


  //try compiler to see what happens;
   Memo3.Lines.Clear;
   Compiler := TCompiler.Create(Scanner);
   try
     Compiler.expression; //???

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

        Memo3.Lines.Add(ByteCode);
     end;
   finally
     Compiler.Free;
   end;


   //spin up compiler again, to show result. At the moment only handles add, plus,divde, multiply. which is why (I think) (5 - (3 - 1)) + -1
   // is giving 4 currently - however, byte code is legit.
   Compiler := TCompiler.Create(Scanner);
   try
     Compiler.expression;
     instructionPointer.Init(Compiler.Chunks);
     VM.Init(InstructionPointer);
     if VM.Run = INTERPRET_OK then 
       Memo3.Lines.Add('result := ' + VM.Result.Value.ToString);
   finally
      VM.Finalize;
     Compiler.Free;
   end;



  finally
    Scanner.finalize;
  end;

end;


end.
