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
    MemStack: TMemo;
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
    procedure LogStackPush(const Value : TValueRecord);
    procedure LogStackPop(const Value : TValueRecord);

  public
    { Public declarations }
  end;


var
  fmScript: TfmScript;

implementation




uses
  dateutils,
  typinfo,
  loxtypes,
  charIterator,
  LineIterator,

  TokenArray,
  compiler,
  //vm,
  newVm, //10 seconds quicker on fib(35)
  //VmMark3,
  Table,
  Locals,
  ValueManager,
  newScanner;

{$R *.dfm}



procedure TfmScript.btnClearClick(Sender: TObject);
begin
  memRun.Lines.clear;
end;


procedure TFmScript.LogStackPush(Const Value : TValueRecord);
begin
  MemStack.Lines.Add(GetString(Value));
  //sleep(500);
  Application.processMessages;

end;

procedure TFmScript.LogStackPop(Const Value : TValueRecord);
begin
  MemStack.Lines.Delete(MemStack.Lines.Count-1);
  //sleep(500);
  Application.processMessages;
end;


procedure TFmScript.Interpret(const LoxFunction : pLoxFunction);
var
   VM :  TVirtualMachine;
   StartTime, EndTime, ElapsedTimeMs: Cardinal;
begin

   VM := TVirtualMachine.Create(MemRun.Lines);
  // Vm.OnPush := LogStackPush;
  // VM.OnPop := LogStackPop;
   try
      StartTime := GetTickCount;
      VM.execute(LoxFunction);
      EndTime := GetTickCount;
      ElapsedTimeMs := EndTime - StartTime;

      MemRun.Lines.Add(inttostr( ElapsedTimeMs) + 'ms');

   finally

     //vm.Free;
     bc.FlushBuffer; //flush out any items owned by the bc. (For now this is probably zero) I need to check :) .

   end;
end;

procedure TfmScript.BtnScanClick(Sender: TObject);
var

  cc : TCompilerController;

  LoxFunction : pLoxFunction;
  c : TCompiler;
  l : TList;

  Tokenizer : TTokenizer;
  Iterator  : TTokenIterator;

begin

  MemRun.Lines.clear;
 // MemStack.Lines.clear;
  MemLocals.Lines.clear;

  try
    Tokenizer.Parse(memEdit.Lines.text);
    Iterator.Init(Tokenizer.Tokens,Tokenizer.TokenCount);

    cc := TCompilerController.Create(Iterator,TYPE_SCRIPT);
    try
     LoxFunction := cc.DoCompile;
     if cc.Stop then exit;
     Interpret(LoxFunction);

   finally
     cc.free;
     //bc.FlushBuffer;
   end;

  finally
    
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
  StartTime, EndTime, ElapsedTimeMs: integer;
begin
  MemRun.Lines.clear;
  StartTime := GetTickCount;
  a := fib(40);
  EndTime := GetTickCount;
  ElapsedTimeMs := EndTime - StartTime;
  MemRun.Lines.Add(inttostr(a));
  MemRun.Lines.Add(inttostr( ElapsedTimeMs) + 'ms');
end;

end.





