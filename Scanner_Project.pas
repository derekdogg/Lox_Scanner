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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Memo3: TMemo;
    procedure BtnScanClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
  loxtypes,
  charIterator,
  LineIterator,
  scanner,
  IntegerArray,
  DoubleArray,
  Stacks,
  chunk,
  TokenArray,
  compiler, vm, ByteCodesArray;

{$R *.dfm}

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
  Iter :  TInstructionPointer;
  constantIndex : byte;
  value : pDouble; //represents a constant
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

     Iter.Init(Compiler.Chunks);
     Memo3.Lines.add('Bytes : ' + inttostr(Iter.ByteCount));
     Memo3.Lines.add('Constants : ' + inttostr(Iter.ConstantCount));

     While Iter.Next <> nil do
     begin
        ByteCode := TOP_Code_name[TOpCodes(Iter.Current^)];
        if (Iter.Current^ = byte(OP_CONSTANT)) then
        begin
          if Iter.Next <> nil then
          begin
            constantIndex := Iter.Current^;
            value := Iter.Constant(constantIndex);
            ByteCode := ByteCode + '=' + floattostr(value^);
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
     Iter.Init(Compiler.Chunks);
     VM.Init(Iter);
     VM.Run;
     Memo3.Lines.Add(floattostr(VM.Result.Value));
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
  A,B,C : TByteCode;
  res : pByteCode;
  ByteCodes : TByteCodes;
  p     : pByteCode;
  i     : integer;
  idx   : integer;

begin
  try
    FillChar(A,Sizeof(A),#0);
    FillChar(B,Sizeof(A),#0);
    FillChar(C,Sizeof(A),#0);

    A.Operation := OP_CONSTANT;
    B.Operation := OP_ADD;
    C.Operation := OP_NIL;

    memo1.Lines.clear;
    p := nil;
    ByteCodes.Init;

    Memo1.lines.add(format('Initial Capacity : %d : ',[ByteCodes.Capacity]));
    Memo1.lines.add(format('Initial slots : %d : ',[ByteCodes.slotcount]));


      Memo1.lines.add(format('Capacity : %d : ',[ByteCodes.Capacity]));
      Memo1.lines.add(format('Free Space before adding item : %d : %d ',[i,ByteCodes.FreeSlots]));
      ByteCodes.Add(A);
      ByteCodes.Remove;
      ByteCodes.Add(B);
      ByteCodes.Remove;
      ByteCodes.Add(C);
      ByteCodes.Remove;
      ByteCodes.Remove;
      ByteCodes.Remove;
      ByteCodes.Remove;
      ByteCodes.Remove;
      ByteCodes.Add(A);
      ByteCodes.Add(A);
      ByteCodes.Add(A);




      //Integers.Add(i/2);

    //  Memo1.lines.add(format('Capacity : %d : ',[Chunk.Capacity]));
    //  Memo1.lines.add(format('Free Space after adding item :%d : %d',[i,Chunk.FreeSpace]));
    Res := ByteCodes.GetItem(0);
    Memo1.lines.add('Count : ' + inttostr(ByteCodes.Count));
    Memo1.lines.add('Op : ' + inttostr(Ord(res.Operation)));

  finally
    ByteCodes.Finalize;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  IntegerStack : TIntegerStack;
  ByteStack : TByteStack;
  ByteCodeStack : TByteCodeStack;
  A,B,C,D,Divide : TByteCode;
  L,R,Result : TByteCode;
  value : Double;
begin
  //10 / 2

  A.Operation := OP_CONSTANT;
  A.Value := 1;
  B.Operation := OP_CONSTANT;
  B.Value := 2;
  C.Operation := OP_CONSTANT;
  C.Value := 3;
  D.Operation := OP_CONSTANT;
  D.Value := 4;

  ByteCodeStack.init;
  ByteCodeStack.Push(A);
  ByteCodeStack.Push(B);

  ByteCodeStack.Pop;
  ByteCodeStack.Pop;

  ByteCodeStack.Push(C);

  result := ByteCodeStack.Pop;

  ByteCodeStack.Finalize;


  //------------- BYTE CODE DIVISION -------------------//


  ByteCodeStack.init;
  ByteCodeStack.Push(A);
  ByteCodeStack.Push(B);


  R := ByteCodeStack.Pop;
  L := ByteCodeStack.Pop;


  Result.Operation := OP_CONSTANT;
  Result.Value := L.value / R.Value; //OP_DIVIDE

  ByteCodeStack.Push(Result);

  Assert(Result.Value = 2);





  ByteCodeStack.Finalize;



(*  IntegerStack.Init;
  IntegerStack.Push(1);
  IntegerStack.Push(2);
  IntegerStack.Push(3);
  IntegerStack.Push(4);


  Memo1.Lines.add(inttostr(IntegerStack.Pop));
  Memo1.Lines.add(inttostr(IntegerStack.Pop));
  Memo1.Lines.add(inttostr(IntegerStack.Pop));
  Memo1.Lines.add(inttostr(IntegerStack.Pop));
  //Memo1.Lines.add(inttostr(IntegerStack.Pop)); //<-- should be -1, i.e. pop is not equivalent to push

  IntegerStack.Finalize;
  *)

  (*ByteStack.Init;
  ByteStack.Push(1);
  ByteStack.Push(2);
  ByteStack.Push(3);
  ByteStack.Push(4);


  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  //Memo1.Lines.add(inttostr(ByteStack.Pop)); //<-- should be -1, i.e. pop is not equivalent to push

  ByteStack.Finalize; *)
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Chunk : TChunks;
  Iter :  TInstructionPointer;
  constantIndex : byte;
  value : pDouble; //represents a constant
  DIter : TDoubleIterator;
  ByteCode : TByteCode;
begin
  try
    Chunk.Init;
  //Chunk.AddReturn;
    Chunk.AddConstant(12.12);  //2 byte
    Chunk.AddADD;
    Chunk.AddConstant(45);     //2 byte
    Chunk.AddReturn;           //1 byte
    Chunk.AddCLOSE_UPVALUE;    //1 byte
    Chunk.AddConstant(55);     //2 byte


 (* DIter.Init(Chunk.Constants);
  While DIter.MoveNext <> nil do
  begin
     Memo1.Lines.Add('Current : ' + floatToStr(DIter.Current^));
  end;
  exit;*)

  Iter.Init(Chunk);
  Memo1.Lines.add('Bytes : ' + inttostr(Iter.ByteCount));
  Memo1.Lines.add('Constants : ' + inttostr(Iter.ConstantCount));

  While Iter.Next <> nil do
  begin
    Memo1.Lines.Add('Current : ' + TOP_Code_name[TOpCodes(Iter.Current^)]);
    if (Iter.Current^ = byte(OP_CONSTANT)) then
    begin
       Memo1.Lines.Add('Constant added');
       if Iter.Next <> nil then
       begin
         constantIndex := Iter.Current^;
         value := Iter.Constant(constantIndex);
         Memo1.Lines.Add(cTab + cTab +'Constant value : ' + floattostr(value^));
       end;
    end;
  end;

 

  finally
    Chunk.Finalize;
  end;



end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Tokens : TTokens;
  Token  : TToken;
begin

end;


procedure TForm1.Button5Click(Sender: TObject);
type
    TValueType = (tkNumber, tkBoolean, tkBytes);
    TValueRecord = record
      Kind : TValueType;
      case tp : TValueType of
        tkNumber : (Number: Double);
        tkBoolean: (True  : boolean);
        tkBytes  : (bytes : array[0..7] of byte);
    end;

var
  value : TValueRecord;
begin
  value.Kind := tkNumber;
  value.Number := 201;
  //  d := 125.56;

//  v := NumberVal(d);

//  Memo2.lines.add('value : ' + floattostr(v.number));
end;

end.



