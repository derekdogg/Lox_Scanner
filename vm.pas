unit vm;

interface
uses
  Chunk,Stacks,
  LOXTypes;

type
  TInterpretResult = (INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  //to do : add in Negate next;

  TVirtualMachine = record
  private
    FInstructionPointer     : TInstructionPointer; //pointer to instructions
    FStack  : TByteCodeStack; //byte code stack
    Procedure Add;
    Procedure Minus;
    Procedure Divide;
    Procedure Multiply;
    Procedure Negate;
    procedure HandleRunTimeError;
  public
    function Result : TByteCode;
    function Run : TInterpretResult;
    procedure init(const IP : TInstructionPointer);
    procedure finalize;
  end;


implementation


{ TVirtualMachine }


function TVirtualMachine.Result: TByteCode;
begin
  assert(FStack.Count = 1);
  result := FStack.Pop;
end;

function TVirtualMachine.Run : TInterpretResult;
var
  ByteCode : TByteCode;
  constantIndex : integer;
  Value : pDouble;

begin
  while FInstructionPointer.Next <> nil do
  begin
    ByteCode.Operation := TOpCodes(FInstructionPointer.Current^);
    if (FInstructionPointer.Current^ = byte(OP_CONSTANT)) then
    begin
      if FInstructionPointer.Next <> nil then
      begin
        constantIndex := FInstructionPointer.Current^;
        value := FInstructionPointer.Constant(constantIndex);
        Bytecode.Value := Value^;
      end;
    end;
    //push constants onto the stack
    if ByteCode.Operation = OP_CONSTANT then
    begin
      FStack.Push(ByteCode);
    end;

    Case ByteCode.Operation of
     OP_ADD : begin
        Add;
     end;

     OP_SUBTRACT : begin
       Minus;
     end;


     OP_DIVIDE : begin
       divide;
     end;

     OP_MULTIPLY : begin
       Multiply;
     end;

     OP_NEGATE : begin
       Negate;
     end;

   end;
  end;
end;


procedure TVirtualMachine.HandleRunTimeError;
begin

end;

procedure TVirtualMachine.Add;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_ADD));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    result.Value := L.Value + R.Value;
    FStack.Push(Result);

  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.Minus;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_SUBTRACT in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_SUBTRACT));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    result.Value := L.Value - R.Value;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.Multiply;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    result.Value := L.Value * R.Value;
    FStack.Push(Result);
  except
     HandleRunTimeError; //<== place holders for now
  end;
end;

procedure TVirtualMachine.Negate;
var
  R : TByteCode;
begin
  //we assume here we're sitting on an OP_NEGATE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    Assert(R.Operation = OP_CONSTANT); //??

    R.Value := - R.Value;
    FStack.Push(R);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.Divide;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    result.Value := L.Value / R.Value;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.finalize;
begin
  FStack.Finalize;
end;

procedure TVirtualMachine.init(const IP : TInstructionPointer);
begin
  FInstructionPointer := IP;
  FStack.Init;
end;



end.
