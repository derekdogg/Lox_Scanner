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
    procedure BtnScanClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

uses loxtypes,charIterator,LineIterator,scanner, IntegerArray, DoubleArray, Stacks, chunk;

{$R *.dfm}

procedure TForm1.BtnScanClick(Sender: TObject);
var
  Scanner : TScanner;
  I : integer;
  Token : TToken;
begin
  Memo2.lines.clear;

  Scanner.Init(Memo1.Lines.Text);
  Scanner.Scan;
  Memo2.Lines.BeginUpdate;
  for i := 0 to Scanner.TokenCount-1  do
  begin
     token := Scanner.Tokens[i];
     Memo2.Lines.Add(TTokenName[token.kind] + '=' + token.text);
  end;

  Memo2.Lines.EndUpdate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Integers : TDoubles;
  p     : pSlotType;
  i     : integer;
  idx   : integer;

begin
  try

    memo1.Lines.clear;
    p := nil;
    Integers.Init;

    Memo1.lines.add(format('Initial Capacity : %d : ',[Integers.Capacity]));
    Memo1.lines.add(format('Initial slots : %d : ',[Integers.slotcount]));

    for i := 1 to 20 do
    begin
      Memo1.lines.add(format('Capacity : %d : ',[Integers.Capacity]));
      Memo1.lines.add(format('Free Space before adding item : %d : %d ',[i,Integers.FreeSlots]));
      Integers.Add(i/2);

    //  Memo1.lines.add(format('Capacity : %d : ',[Chunk.Capacity]));
    //  Memo1.lines.add(format('Free Space after adding item :%d : %d',[i,Chunk.FreeSpace]));
    end;

   (* memo1.Lines.beginupdate;
    Memo1.lines.add('Resized : ' + inttostr(Chunk.ResizeCount));
    Memo1.lines.add('Free Space after adding 8 items : ' + inttostr(Chunk.FreeSlots));
    for i := 0 to Chunk.Count-1 do
    begin
      p := Chunk.Item(i);
      if p^ <> i then
      begin

        break
      end;
      memo1.Lines.add(inttostr(p^));
    end;
    memo1.lines.endupdate; *)
  finally
    Integers.Finalize;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  IntegerStack : TIntegerStack;
  ByteStack : TByteStack;


begin
  IntegerStack.Init;
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


  ByteStack.Init;
  ByteStack.Push(1);
  ByteStack.Push(2);
  ByteStack.Push(3);
  ByteStack.Push(4);


  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  Memo1.Lines.add(inttostr(ByteStack.Pop));
  //Memo1.Lines.add(inttostr(ByteStack.Pop)); //<-- should be -1, i.e. pop is not equivalent to push

  ByteStack.Finalize;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Chunk : TChunk;
  Iter :  TChunkIterator;

begin
  Chunk.Init;
  Chunk.AddReturn;
  Chunk.AddConstant(12.12);
  Chunk.AddReturn;
  Chunk.AddCLOSE_UPVALUE;
  Iter.Init(Chunk);

  While Iter.Next <> OP_NULL do
  begin
    Memo1.Lines.Add('Current : ' + TOP_Code_name[Iter.Current]);
    Memo1.Lines.Add('Peek next : ' + TOP_Code_name[Iter.PeekNext]);
  end;

  Chunk.Finalize;

end;

end.


