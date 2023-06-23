unit vm;

interface
uses
  Chunk;

type
  TInterpretResult = (INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  TVirtualMachine = record
  private
    FOpCodeIterator : TInstructionPointer;
    FChunk  : TChunks;
  public
    function Run : TInterpretResult;
    procedure init;
    procedure finalize;
  end;


implementation
uses
  LOXTypes;

{ TVirtualMachine }


function TVirtualMachine.Run : TInterpretResult;
var
  instruction : TOPCodes;
begin
(*  result := INTERPRET_RUNTIME_ERROR;
  repeat
    instruction := FOpCodeIterator.Next;
    Case FOpCodeIterator.Next of
      OP_RETURN: Begin
        result := INTERPRET_OK;
        exit;
      end;
    end;
  until Instruction = OP_NULL;  *)
end;


procedure TVirtualMachine.finalize;
begin
  FChunk.Finalize;
end;

procedure TVirtualMachine.init;
begin
  FChunk.Init;
  FOpCodeIterator.Init(FChunk);
end;

end.
