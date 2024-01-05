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
  scanner,

  TokenArray,
  compiler,
  //vm,
  newVm, //10 seconds quicker on fib(35)
  //VmMark3,
  Table,
  Locals,
  ValueManager;

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
  Scanner : TScanner;
  cc : TCompilerController;
  Tokens : TTokenIterator;
  LoxFunction : pLoxFunction;
  c : TCompiler;
  l : TList;

begin

  MemRun.Lines.clear;
 // MemStack.Lines.clear;
  MemLocals.Lines.clear;

  try
   //MemRun.Lines.BeginUpdate;
   Scanner.Init(MemEdit.Lines.Text);
   Scanner.Scan;
   Tokens.Init(Scanner.Tokens);

   cc := TCompilerController.Create(Tokens,Scanner,TYPE_SCRIPT);
   try
     LoxFunction := cc.DoCompile;
     if cc.Stop then exit;
     Interpret(LoxFunction);

   finally
     cc.free;
     //bc.FlushBuffer;
   end;

  finally
    Scanner.finalize;
    //MemRun.Lines.EndUpdate;
  end;

end;


procedure TfmScript.Button1Click(Sender: TObject);

  function fib(n : single) : single;
  begin
    //Memstack.Lines.add(inttostr(n));
    if (n < 2) then
    begin


      result := n;

      exit;
    end;
     
    result := fib(n - 2) + fib(n - 1);
  end;

var
  a : single;
  StartTime, EndTime, ElapsedTimeMs: Cardinal;
begin
  MemRun.Lines.clear;
  StartTime := GetTickCount;
  a := fib(40);
 EndTime := GetTickCount;
      ElapsedTimeMs := EndTime - StartTime;

      MemRun.Lines.Add(inttostr( ElapsedTimeMs) + 'ms');



end;

end.





