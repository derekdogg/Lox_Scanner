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
    procedure BtnScanClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
    procedure Interpret(const LoxFunction : pLoxFunction);

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



procedure TFmScript.Interpret(const LoxFunction : pLoxFunction);
var
  VM :  TVirtualMachine;
begin
   
   try
      VM.Init(LoxFunction,MemRun.Lines,nil{ MemCodes.Lines});
      VM.Run;

   finally

     vm.Finalize;
   end;
end;

procedure TfmScript.BtnScanClick(Sender: TObject);
var
  Scanner : TScanner;
  cc : TCompilerController;
  Tokens : TTokenIterator;

  LoxFunction : pLoxFunction;
  i : integer;
  value : pValue;
  c : TCompiler;

begin


  //instructionPointer.Init(LoxFunction);
  MemRun.Lines.clear;
  MemCodes.Lines.clear;
  MemLocals.Lines.clear;

  try

   Scanner.Init(MemEdit.Lines.Text);
   Scanner.Scan;
   Tokens.Init(Scanner.Tokens);

   cc := TCompilerController.Create(Tokens,Scanner,TYPE_SCRIPT);
   try
     LoxFunction := cc.DoCompile;
     Interpret(LoxFunction);
   finally
     cc.free;
   end;

  finally
    Scanner.finalize;

  end;

end;


end.
