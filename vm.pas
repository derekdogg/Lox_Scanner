unit vm;

interface

uses
  sysutils,classes,Chunk,Stacks,
  LOXTypes;

type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  //to do : add in Negate next;

  TVirtualMachine = record
  private
    FInstructionPointer     : TInstructionPointer; //pointer to instructions
    FStack  : TByteCodeStack; //byte code stack
    FResults : TStrings;
    Procedure Add;
    Procedure subtract;
    Procedure Divide;
    Procedure Multiply;
    Procedure Negate;
    procedure Print;
    procedure Equal;
    procedure Greater;
    procedure Less;
    procedure NotEqual;
    procedure DoTrue;
    procedure DoFalse;
    procedure DoNil;
    procedure HandleRunTimeError(const E: Exception);
    Function isFalsey(value : TValue) : Boolean;
  public
    function Result : TByteCode;
    function Run : TInterpretResult;
    procedure init(const IP : TInstructionPointer; const results : TStrings);
    procedure finalize;
  end;


implementation
uses
  dialogs;


{ TVirtualMachine }
function TVirtualMachine.Result: TByteCode;
begin
  assert(FStack.Count = 1, 'stack is empty');
  result := FStack.Pop;
end;

function TVirtualMachine.Run : TInterpretResult;
var
  ByteCode : TByteCode;
  constantIndex : integer;
  Value : pValue;

begin
  Result := INTERPRET_NONE;
  if FInstructionPointer.ByteCount = 0 then exit;
  while FInstructionPointer.Next <> nil do
  begin
    ByteCode.Operation := TOpCodes(FInstructionPointer.Current^);

    Case ByteCode.Operation of

      OP_CONSTANT : begin

           if FInstructionPointer.Next = nil then raise exception.create('Expected constant value following constant operation');
           constantIndex := FInstructionPointer.Current^;
           value := FInstructionPointer.value(constantIndex);
           Bytecode.Value := Value^;
           FStack.Push(ByteCode);
      end;

      OP_Nil  : begin
        DoNil;
      end;

      OP_TRUE : begin
         DoTrue;
      end;

      OP_FALSE : begin
         DoFalse;
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

      OP_PRINT  : begin
          Print;
      end;
    end;
  end;
  Result := INTERPRET_OK;
end;


procedure TVirtualMachine.HandleRunTimeError(const E : Exception);
begin
  showmessage(E.message);
end;

procedure TVirtualMachine.Add;
var
  L,R : TByteCode;
begin
  Assert(FStack.Peek(0).Operation = OP_CONSTANT, 'Trying to Add - STACK[0] is not OP_CONSTANT');
  Assert(FStack.Peek(1).Operation = OP_CONSTANT, 'Trying to Add - STACK[0] is not OP_CONSTANT');
  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    L.Operation := OP_CONSTANT;

    if (L.Value.IsNumber) and (R.Value.IsNumber) then
    begin
      L.Value.Number := L.Value.Number + R.Value.Number;
      FStack.Push(L);
      exit;
    end;

    if (L.Value.IsStringObject) and (R.Value.IsStringObject) then
    begin
      //What goes here?
      L.Value.Str := L.Value.ToString + R.Value.ToString;
      FStack.Push(L);
    end;
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.subtract;
var
  L,R : TByteCode;
begin
  Assert(FStack.Peek(0).Operation = OP_CONSTANT, 'Stack 0 Current operation is not Constant');
  Assert(FStack.Peek(1).Operation = OP_CONSTANT, 'Stack 1 Current operation is not Constant');
  Assert(FInstructionPointer.Current^ = byte(OP_SUBTRACT));
  try
    R := FStack.Pop;
    L := FStack.Pop;

    if (L.Value.IsNumber) and (R.Value.IsNumber) then
    begin
      L.Value.number := L.Value.Number - R.Value.Number;
      FStack.Push(L);
      exit;
    end;

    if (L.Value.IsStringObject) and (R.Value.IsStringObject) then
    begin
      L.Value.Str := StringReplace(l.Value.tostring,r.value.tostring, '', [rfReplaceAll]);    //this is a bit of a curiosity, it doesn't just deduct 1 time, it does it multiple times...
      FStack.Push(L);
    end;
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Multiply;
var
  L,R : TByteCode;
  i : integer;
  s : string;

begin
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    if (L.Value.IsNumber) and (R.Value.IsNumber) then
    begin

      L.Value.Number := L.Value.Number * R.Value.Number;
      FStack.Push(L);
      exit;
    end;

     if (L.Value.IsStringObject) and (R.Value.IsNumber) then
     begin
       s := '';
       for i := 0 to round(R.Value.Number-1) do
       begin
         s := s + L.Value.Str
       end;
       L.Value.Str := s;
       FStack.Push(L);
     end;

     if (R.Value.IsStringObject) and (L.Value.IsNumber) then
     begin
       s := '';
       for i := 0 to round(L.Value.Number-1) do
       begin
         s := s + R.Value.Str
       end;
       R.Value.Str := s;
       FStack.Push(R);
     end;

     //code duplication

 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Print;
begin
  Assert(FInstructionPointer.Current^ = byte(OP_PRINT));
  FResults.Add('PRINT:' + FStack.Pop.Value.ToString);
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

    if (R.Value.IsNumber) then
    begin
      R.Value.Number := - R.Value.Number;
      FStack.Push(R);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
    end;



  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Divide;
var
  L,R : TByteCode;
begin
  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    Assert(R.Value.Number <> 0); //divide by zero exceptions.
    L := FStack.Pop;
    L.Value.Number := L.Value.Number / R.Value.Number;
    FStack.Push(L);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

Function TVirtualMachine.isFalsey(value : TValue) : Boolean;
begin
   result := (Value.Kind = lxNull) OR ((Value.Kind = lxBoolean) and Value.Boolean = false);
end;

{  C code...
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

procedure TVirtualMachine.NotEqual;
var
  Result : TByteCode;
begin
  //Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  //Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  Assert(FInstructionPointer.Current^ = byte(OP_NOT), 'Current instruction is <> NOT');
  try
    result.Value.Boolean := isFalsey(FStack.pop.value);
    FStack.Push(Result);

   except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R, Result : TByteCode;
begin
   //Assert(FStack.Peek(0).Operation = OP_CONSTANT);
   //Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    Result.Value.Boolean := r.Value.ToString = l.value.ToString;
    FStack.Push(result);

  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.finalize;
begin
  FStack.Finalize;
end;

procedure TVirtualMachine.DoTrue;
var
  ByteCode : TByteCode;
begin
  ByteCode.Operation := OP_TRUE;
  ByteCode.Value.Boolean := true;
  FStack.Push(ByteCode);
end;


procedure TVirtualMachine.DoFalse;
var
  ByteCode : TByteCode;
begin
  ByteCode.Operation := OP_FALSE;
  ByteCode.Value.Boolean := false;
  FStack.Push(ByteCode);
end;

procedure TVirtualMachine.DoNil;
var
  ByteCode : TByteCode;
begin
  ByteCode.Operation := OP_NULL;
  ByteCode.Value.Null := true;
  FStack.Push(ByteCode);
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
    if l.Value.Number > r.value.Number then
      result.value.Boolean := true
    else
      result.value.Boolean := false;
    FStack.Push(Result);
  except on E:exception do
     HandleRunTimeError(e);
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
    if l.Value.Number < r.value.Number then
      result.value.Boolean := true
    else
      result.value.Boolean := false;
    FStack.Push(Result);
 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.init(const IP : TInstructionPointer; const results : TStrings);
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');
  FResults := results;
  FInstructionPointer := IP;
  FStack.Init;
end;




end.
