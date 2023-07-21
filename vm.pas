unit vm;

interface

uses
  sysutils,classes,Chunk,Stacks,
  LOXTypes, Table, ValueList;

type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  //to do : add in Negate next;

  TVirtualMachine = record
  private
    FInstructionPointer  : TInstructionPointer; //pointer to instructions
    FStackResults : TValueList;  //keep track of new values added to stack.For disposal later.
    FStack  : TValueStack; //byte code stack
    FGlobals : TValuePairs;

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
    procedure DoGlobal;
    procedure DoGetGlobal;
    procedure DoSetGlobal;
    procedure DoPOP;
    procedure HandleRunTimeError(const E: Exception);
    Function isFalsey(value : pValue) : Boolean;
  public
//    function Result : TByteCode;
    function Run : TInterpretResult;
    procedure init(const IP : TInstructionPointer; const results : TStrings);
    procedure finalize;
  end;


implementation
uses
  dialogs;


{ TVirtualMachine }
(*function TVirtualMachine.Result: TByteCode;
begin
  assert(FStack.Count = 1, 'stack is empty');
  result := FStack.Pop;
end;*)



function TVirtualMachine.Run : TInterpretResult;
var
 // ByteCode : TByteCode;
  constantIndex : integer;
  Value : pValue;
  Name  : pValue;
  v : TValue;
  stackCount : integer;
begin
  Result := INTERPRET_NONE;
  if FInstructionPointer.ByteCount = 0 then exit;
  while FInstructionPointer.Next <> nil do
  begin

    //ByteCode.Operation := TOpCodes(FInstructionPointer.Current^);

    Case TOpCodes(FInstructionPointer.Current^) of

      OP_CONSTANT : begin
           if FInstructionPointer.Next = nil then raise exception.create('Expected constant value following constant operation');
           constantIndex := FInstructionPointer.Current^;
           value := FInstructionPointer.value(constantIndex);
           //Bytecode.Value := Value;
           FStack.Push(value);
      end;


      OP_DEFINE_GLOBAL: begin
          DoGlobal;
          { ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break; }

      end;

      OP_POP : Begin
          DoPOP;
      end;

      
      OP_SET_GLOBAL:
      begin
        DoSetGlobal;

      end;



      OP_GET_GLOBAL:
      begin
        doGetGlobal;

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
    StackCount := FStack.Count;
  end;
  Result := INTERPRET_OK;
end;


procedure TVirtualMachine.HandleRunTimeError(const E : Exception);
begin
  showmessage(E.message);
end;

procedure TVirtualMachine.Add;
var
  L,R : pValue;

begin
  //Assert(FStack.Peek(0).Operation = OP_CONSTANT, 'Trying to Add - STACK[0] is not OP_CONSTANT');
  //Assert(FStack.Peek(1).Operation = OP_CONSTANT, 'Trying to Add - STACK[0] is not OP_CONSTANT');
  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
   // A := FStack.Pop;
//    B := FStack.Pop;

    //L.Operation := OP_CONSTANT;

    if (L.IsNumber) and (R.IsNumber) then
    begin
      L.Number := L.Number + R.Number;
      FStack.Push(L);
      exit;
    end;

    if (L.IsStringObject) and (R.IsStringObject) then
    begin
      //What goes here?
      L.Str := L.ToString + R.ToString;
      FStack.Push(L);
    end;
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.subtract;
var
  L,R : pValue;
begin
  //Assert(FStack.Peek(0).Operation = OP_CONSTANT, 'Stack 0 Current operation is not Constant');
  //Assert(FStack.Peek(1).Operation = OP_CONSTANT, 'Stack 1 Current operation is not Constant');


  Assert(FInstructionPointer.Current^ = byte(OP_SUBTRACT));
  try
    R := FStack.Pop;
    L := FStack.Pop;

    if (L.IsNumber) and (R.IsNumber) then
    begin
      L.number := L.Number - R.Number;
      FStack.Push(L);
      exit;
    end;

    if (L.IsStringObject) and (R.IsStringObject) then
    begin
      L.Str := StringReplace(l.tostring,r.tostring, '', [rfReplaceAll]);    //this is a bit of a curiosity, it doesn't just deduct 1 time, it does it multiple times...
      FStack.Push(L);
    end;
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Multiply;
var
  L,R : pValue;
  i : integer;
  s : string;

begin
//  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
 // Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    if (L.IsNumber) and (R.IsNumber) then
    begin

      L.Number := L.Number * R.Number;
      FStack.Push(L);
      exit;
    end;

     if (L.IsStringObject) and (R.IsNumber) then
     begin
       s := '';
       for i := 0 to round(R.Number-1) do
       begin
         s := s + L.Str
       end;
       L.Str := s;
       FStack.Push(L);
     end;

     if (R.IsStringObject) and (L.IsNumber) then
     begin
       s := '';
       for i := 0 to round(L.Number-1) do
       begin
         s := s + R.Str
       end;
       R.Str := s;
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
  FResults.Add('PRINT:' + FStack.Pop.ToString);
end;

procedure TVirtualMachine.Negate;
var
  R : pValue;
begin
//  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_NEGATE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    //Assert(R.Operation = OP_CONSTANT); //??

    if (R.IsNumber) then
    begin
      R.Number := - R.Number;
      FStack.Push(R);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
    end;



  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Divide;
var
  L,R : pValue;
begin
//  Assert(FStack.Peek(0).Operation = OP_CONSTANT);
//  Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    Assert(R.Number <> 0); //divide by zero exceptions.
    L := FStack.Pop;
    L.Number := L.Number / R.Number;
    FStack.Push(L);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

Function TVirtualMachine.isFalsey(value : pValue) : Boolean;
begin
   result := (Value.Kind = lxNull) OR ((Value.Kind = lxBoolean) and Value.Boolean = false);
end;

{  C code...
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

procedure TVirtualMachine.NotEqual;
var
  Result : pValue;
begin
  //Assert(FStack.Peek(0).Operation = OP_CONSTANT);
  //Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  Assert(FInstructionPointer.Current^ = byte(OP_NOT), 'Current instruction is <> NOT');
  try
    result := NewBool(isFalsey(FStack.pop));
    FStack.Push(Result);
    FStackResults.Add(Result);
   except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R, Result : pValue;
begin
   //Assert(FStack.Peek(0).Operation = OP_CONSTANT);
   //Assert(FStack.Peek(1).Operation = OP_CONSTANT);
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    Result := NewBool(r.ToString = l.ToString);
    FStack.Push(result);
    FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.DoTrue;
var
  value : pValue;
begin
  value := newBool(true);
  FStack.Push(value);
end;


procedure TVirtualMachine.DoFalse;
var
  Value : pValue;
begin
  Value := NewBool(False);
  FStack.Push(Value);
end;

procedure TVirtualMachine.DoNil;
var
  value : pValue;
begin
  new(Value);
  Value.Null := true;
  FStack.Push(value);
end;



(*
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break; *)
procedure TVirtualMachine.DoGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   NameValue : pNameValue;

begin
  assert(FInstructionPointer.Current^ = byte(OP_Get_GLOBAL), 'current instruction is not op define global');
  if FInstructionPointer.Next <> nil then
  begin
     constantIndex := FInstructionPointer.Current^;
     name := FInstructionPointer.Value(constantIndex);
     NameValue := FGlobals.Find(name.tostring);
     Assert(NameValue <> nil, 'expected value does not exist in globals');
     FStack.push(NameValue.Value);
  end;
  //FInstructionPointer.Next;
end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin
  assert(FInstructionPointer.Current^ = byte(OP_SET_GLOBAL), 'current instruction is not op set global');
        (*
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
      *)
  if FInstructionPointer.Next <> nil then
  begin
     constantIndex := FInstructionPointer.Current^;
     name := FInstructionPointer.Value(constantIndex);
     value := FStack.Peek(0);
     assert(name.IsStringObject, 'name is not a string object');
     NameValue := FGlobals.Find(name.tostring);
     assert(NameValue <> nil, 'Could not locate global in entries to set its new value');
     NameValue.Value := Value;
  end;

end;



(*

  if InstructionPointer.Next <> nil then
          begin
            constantIndex := InstructionPointer.Current^;
            value := InstructionPointer.Value(constantIndex);
            ByteCode := ByteCode + '=' + value.ToString;
          end;

*)

procedure TVirtualMachine.DoPOP;
begin
   FStack.pop;
end;

procedure TVirtualMachine.DoGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;
   NameValue : pNameValue;
//   bcode : pByteCode;
   
begin
  assert(FInstructionPointer.Current^ = byte(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  if FInstructionPointer.Next <> nil then
  begin
     constantIndex := FInstructionPointer.Current^;
     name := FInstructionPointer.Value(constantIndex);
     value := FStack.Peek(0);
     assert(name.IsStringObject, 'name is not a string object');

     if FGlobals.Find(Name.ToString) <> nil then exit; //already exists


     NameValue := NewValuePair(Name,value);
     assert(FGlobals.Add(NameValue) = true, 'failed to add to hash table');

     //bCode := FStack.Top;
     FStack.pop;
  end;
end;

procedure TVirtualMachine.Greater;
var
  L,R,Result : pValue;
begin
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_GREATER));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    Result := NewBool(l.Number > r.Number);
    FStack.Push(result);
    FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Less;
var
  L,R, Result : pValue;
begin
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_LESS));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;
    Result := NewBool(l.Number < r.Number);
    FStack.Push(Result);
    FStackResults.Add(Result);
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
  FGlobals.Init;
  FStackResults.Init(true);
end;


procedure TVirtualMachine.finalize;
begin
  FStack.Finalize;
  FGlobals.Finalize;
  FStackResults.Finalize;
end;




end.
