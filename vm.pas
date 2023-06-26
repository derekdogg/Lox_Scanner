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
    Procedure subtract;
    Procedure Divide;
    Procedure Multiply;
    Procedure Negate;
    procedure Equal;
    procedure Greater;
    procedure Less;
    procedure NotEqual;
    procedure HandleRunTimeError;
  public
    function Result : TByteCode;
    function Run : TInterpretResult;
    procedure init(const IP : TInstructionPointer);
    procedure finalize;
  end;


implementation
uses
  sysutils;


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

    Case ByteCode.Operation of

      OP_CONSTANT : begin

           if FInstructionPointer.Next = nil then raise exception.create('Expected constant value following constant operation');
           constantIndex := FInstructionPointer.Current^;
           value := FInstructionPointer.Constant(constantIndex);
           Bytecode.Value := Value^;
           FStack.Push(ByteCode);
      end;


      OP_GREATER : begin
         greater;
      end;

      OP_LESS : begin
         less;
      end;

      OP_EQUAL : begin
        equal;
      end;

      OP_NOT : begin
        NotEqual;
      end;


      OP_ADD : begin
        Add;
      end;

      OP_SUBTRACT : begin
        subtract;
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
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
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

procedure TVirtualMachine.subtract;
var
  L,R,Result : TByteCode;
begin

  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);

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
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
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
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
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
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    Assert(R.Value <> 0); //divide by zero exceptions.
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    result.Value := L.Value / R.Value;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.NotEqual;
var
  Result : TByteCode;
begin
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  Assert(FInstructionPointer.Current^ = byte(OP_NOT));
  try
    result := FStack.Pop;
    Result.Value := -Result.Value;
    FStack.Push(Result);
   except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_EQUAL));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    if r.Value = l.value then
      result.value := 1
    else
      result.value := -1;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.finalize;
begin
  FStack.Finalize;
end;

procedure TVirtualMachine.Greater;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_GREATER));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    if l.Value > r.value then
      result.value := 1
    else
      result.value := -1;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;


procedure TVirtualMachine.Less;
var
  L,R,Result : TByteCode;
begin
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_LESS));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result.Operation := OP_CONSTANT;
    if l.Value < r.value then
      result.value := 1
    else
      result.value := -1;
    FStack.Push(Result);
  except
     HandleRunTimeError;
  end;
end;

procedure TVirtualMachine.init(const IP : TInstructionPointer);
begin
  FInstructionPointer := IP;
  FStack.Init;
end;




end.
