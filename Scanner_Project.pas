unit Scanner_Project;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Values;

type

  TfmScript = class(TForm)
    Panel1: TPanel;
    BtnScan: TButton;
    btnClear: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    MemLocals: TMemo;
    MemRun: TMemo;
    MemCodes: TMemo;
    Panel6: TPanel;
    memEdit: TRichEdit;
    Splitter1: TSplitter;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Button1: TButton;
    procedure BtnScanClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Interpret(const LoxFunction : pLoxFunction);
    procedure LogStackPush(Const stack : TStack);
    procedure LogStackPop(Const stack : TStack);

  public
    { Public declarations }
  end;


var
  fmScript: TfmScript;

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
  ValueManager;

{$R *.dfm}



procedure TfmScript.btnClearClick(Sender: TObject);
begin
  memEdit.Lines.clear;
end;


procedure TFmScript.LogStackPush(Const stack : TStack);
begin
  Edit1.Text := inttostr(Stack.StackTop);
  Edit2.Text := inttostr(Stack.Capacity);
  Application.processMessages;

end;

procedure TFmScript.LogStackPop(Const stack : TStack);
begin
  Edit1.Text := inttostr(Stack.StackTop);
  Edit2.Text := inttostr(Stack.Capacity);
  Application.processMessages;
end;


procedure TFmScript.Interpret(const LoxFunction : pLoxFunction);
var
   VM :  TVirtualMachine;
begin


   VM := TVirtualMachine.Create(MemRun.Lines);
   try
      MemRun.Lines.Add(DateTimeToStr(now));
      VM.Run(LoxFunction);
   finally
     MemRun.Lines.Add(DateTimeToStr(now));
     vm.Free;
     BorrowChecker.FlushBuffer; //flush out any items owned by the BorrowChecker. (For now this is probably zero) I need to check :) .

   end;
end;

procedure TfmScript.BtnScanClick(Sender: TObject);
var
  Scanner : TScanner;
  cc : TCompilerController;
  Tokens : TTokenIterator;
  LoxFunction : pLoxFunction;
  c : TCompiler;
  l : TList;

begin




 // BorrowChecker.FlushBuffer;
  //BorrowChecker.Logger := MemRun.Lines;

  //instructionPointer.Init(LoxFunction);
  MemRun.Lines.clear;
  MemCodes.Lines.clear;
  MemLocals.Lines.clear;

  try
   //MemRun.Lines.BeginUpdate;
   Scanner.Init(MemEdit.Lines.Text);
   Scanner.Scan;
   Tokens.Init(Scanner.Tokens);

   cc := TCompilerController.Create(Tokens,Scanner,TYPE_SCRIPT);
   try
     LoxFunction := cc.DoCompile;
     Interpret(LoxFunction);

   finally
     cc.free;
     //BorrowChecker.FlushBuffer;
   end;

  finally
    Scanner.finalize;
    //MemRun.Lines.EndUpdate;
  end;

end;


procedure TfmScript.Button1Click(Sender: TObject);

  function fib(n : integer) : integer;
  begin
    if (n < 2) then
    begin
      result := n;

      exit;
    end;
    
    result := fib(n - 2) + fib(n - 1);
  end;



var
  a : integer;
begin
  MemRun.Lines.clear;
  MemRun.Lines.beginUpdate;
  a := fib(40);
  MemRun.Lines.add(inttostr(a));


  MemRun.Lines.EndUpdate;
end;

end.

