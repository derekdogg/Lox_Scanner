unit vm;

interface

uses
  sysutils,
  classes,
  LOXTypes,
  Table,
  values,
  opcodes,
  natives;

const
   Frames_Max = 128;
   Stack_Max = Frames_Max * 256;


type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  TVirtualMachine = record
  private
    FCurrentFrame : TCallFrame;

    FHalt    : boolean;
    FNatives : TNatives;
    FFrames : TCallFrames;


    FStackResults : TValueList;  //keep track of new values added to stack.For disposal later.

    FGlobals : TValuePairs;

    FResults : TStrings;
    FLog : TStrings;

     
    procedure ClearLog;
    procedure Log(Const value : String);overload;
    procedure Log(Const opcode : TopCodes);overload;
    procedure Log(Const opCOde : TopCodes; const operand : integer); overload;
    procedure Log(const opCOde : TopCodes;const operand : integer;const value   : pValue); overload;
    procedure Log(const OpCode : TOpCodes; const L,R : pValue);overload;

    function VMStack : TValueStack;

    function PopStack : pValue;
    function PeekStack : pValue;
    procedure MoveNext;
    procedure DoConstant;
    Procedure DoAdd;
    Procedure subtract;
    Procedure Divide;
    Procedure Multiply;
    Procedure Negate;
    procedure Print;
    procedure Loop;
    procedure Equal;
    procedure Greater;
    procedure Less;
    procedure NotEqual;
    procedure DoTrue;
    procedure DoFalse;
    procedure DoNil;
    procedure DoGlobal;
    procedure DoGetGlobal;
    procedure RegisterNatives;
    procedure DoSetGlobal;
    procedure DoPOP;
    procedure OpGetLocal;
    procedure OPSetLocal;
    procedure HandleRunTimeError(const E: Exception);
    procedure JumpFalse;
    procedure Jump;
    function Call(const Func : pLoxfunction; const ArgCount : integer) : boolean;
    procedure DoCall;
    procedure DoReturn;
    function CallValue(const callee : pValue; ArgCount : integer) : boolean;
    Function isFalsey(value : pValue) : Boolean;
    procedure AddGlobal(const name : pValue ; const Value : pValue);

  public
//    function Result : TByteCode;
    function Run : TInterpretResult;

    procedure init(
       const LoxFunction : pLoxFunction;
       const results : TStrings;
       const Log     : TStrings);
     procedure finalize;

  end;

implementation

uses
  dialogs, addition, subtraction;


{ TVirtualMachine }
(*function TVirtualMachine.Result: TByteCode;
begin
  assert(FFrames.Stack.Count = 1, 'stack is empty');
  result := CurrentFrame.Stack.Pop;
end;*)

function TVirtualMachine.Run : TInterpretResult;
begin

  //CallFrame* frame = &vm.frames[vm.frameCount - 1];

  clearLog;


  Result := INTERPRET_NONE;
  if FCurrentFrame.InstructionPointer.Count = 0 then exit;
  while (FHalt = false) and (FCurrentFrame.InstructionPointer.Next <> nil) do
  begin

    Case TOpCodes(FCurrentFrame.InstructionPointer.CurrentByte^) of

      OP_CONSTANT : begin
         DoConstant;
      end;


      OP_DEFINE_GLOBAL: begin
          Log(OP_DEFINE_GLOBAL);
          DoGlobal;
       end;

      OP_POP : Begin
          Log(OP_POP);
         DoPOP;
      end;


      OP_SET_GLOBAL:
      begin
       Log(OP_SET_GLOBAL);
        DoSetGlobal;
      end;



      OP_GET_GLOBAL:
      begin
        Log(OP_GET_GLOBAL);
        doGetGlobal;
      end;


      OP_GET_LOCAL:
      begin

        OpGetLocal;
      end;

      OP_SET_LOCAL:
      begin
        Log(OP_SET_LOCAL);
        OPSetLocal;
      end;


      OP_Nil  : begin
          Log(OP_Nil);
         DoNil;
      end;

      OP_TRUE : begin
         Log(OP_TRUE);
         DoTrue;
      end;

      OP_FALSE : begin
          Log(OP_FALSE);
         DoFalse;
      end;

      OP_GREATER : begin
          Log(OP_GREATER);
         greater;
      end;

      OP_LESS : begin
          Log(OP_LESS);
         less;
      end;

      OP_EQUAL : begin
         Log(OP_EQUAL);
        equal;
      end;

      OP_NOT : begin
       Log(OP_NOT);
        NotEqual;
      end;


      OP_ADD : begin
         Log(OP_ADD);
        DoAdd;
      end;

      OP_SUBTRACT : begin
        subtract;
      end;


      OP_DIVIDE : begin
         Log(OP_DIVIDE);
        divide;
      end;

      OP_MULTIPLY : begin
         Log(OP_MULTIPLY);
         Multiply;
      end;

      OP_NEGATE : begin
         Log(OP_NEGATE);
         Negate;
      end;

      OP_PRINT  : begin
         Log(OP_PRINT);
         Print;
      end;


      OP_JUMP_IF_FALSE:

      begin
         Log(OP_JUMP_IF_FALSE);
         JumpFalse;
      end;


      OP_JUMP:
      begin
         Log(OP_JUMP);
         Jump;
      end;


       OP_LOOP:
       begin
         Log(OP_LOOP);
         loop;
       end;

      OP_CALL :
      begin

         DoCall; //note here this will create a new stack frame, create a new IP. Something to be aware of.
      end;


      OP_Return :
      begin
         Log(OP_Return);
         DoReturn;
      end;

    end;

  end;
  Result := INTERPRET_OK;
end;




procedure TVirtualMachine.HandleRunTimeError(const E : Exception);
begin
  showmessage(E.message);
  FHalt := true;
end;

procedure TVirtualMachine.DoConstant;
var
  constantIndex : integer;
  value : pValue;
begin
   assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_CONSTANT));

   MoveNext;

   constantIndex := FCurrentFrame.InstructionPointer.CurrentByte^;

   value := FCurrentFrame.InstructionPointer.Constant[constantIndex];
   VMStack.Push(value);

   Log(OP_CONSTANT,ConstantIndex,value);

end;


procedure TVirtualMachine.Greater;
var
  L,R,Result : pValue;
begin

  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_GREATER));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VmStack.Pop;
    L := VmStack.Pop;

    Result := NewBool(l.Number > r.Number);
    VMStack.Push(result);
   // FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Less;
var
  L,R, Result : pValue;
begin

  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_LESS));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.pop;
    L := VMStack.Pop;
    Result := NewBool(l.Number < r.Number);
    VMStack.Push(Result);
    //FStackResults.Add(Result);
 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.DoAdd;
var
  L,R : pValue;
  Result : pValue;
begin
//  result := nil;

  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.Pop;
    L := VMStack.Pop;

    result := TAddition.Add(L,R);
//    if assigned(result) then
    VMStack.Push(result);

  except on E:exception do                     
     HandleRunTimeError(e);
  end;
end;


 

procedure TVirtualMachine.Log(
  const OpCode : TOpCodes; const L,R : pValue);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode) + '. LEFT : ' + L.ToString + ' RIGHT:' + R.ToString);

end;

procedure TVirtualMachine.subtract;
var
  Result, L,R : pValue;

begin
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_SUBTRACT));

  R := VMStack.Pop;
  L := VMStack.Pop;

  Log(OP_SUBTRACT,L,R);

  result := TSubtraction.Subtract(L,R);

  VMStack.Push(result);

end;

procedure TVirtualMachine.Multiply;
var
  L,R : pValue;
  i : integer;
  s : string;

begin
(*
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.Pop;
    L := VMStack.Pop;

    if (L.IsNumber) and (R.IsNumber) then
    begin

      L.Number := L.Number * R.Number;
      VMStack.Push(L);
      exit;
    end;

     if (L.IsString) and (R.IsNumber) then
     begin
       s := '';
       for i := 0 to round(R.Number-1) do
       begin
         s := s + L.Str
       end;
       L.Str := s;
       VMStack.Push(L);
     end;

     if (R.IsString) and (L.IsNumber) then
     begin
       s := '';
       for i := 0 to round(abs(L.Number))-1 do
       begin
         s := s + R.Str
       end;
       R.Str := s;
       VMStack.Push(R);
     end;

     //code duplication

 except on E:exception do
     HandleRunTimeError(e);
  end; *)
end;

procedure TVirtualMachine.Print;
var
  value : pValue;
begin

  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_PRINT));
  //value := FCurrentFrame.Stack.Pop;
  value := VMStack.Pop;

  FResults.Add('PRINT:' + Value.ToString);
end;


function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : integer) : boolean;
var
  prevOffset : integer;
  newStackTop : integer;
begin

  result := false;

  if not (argCount = func.Arity) then raise exception.create('param mismatch');

  prevOffset :=  0;

  FCurrentFrame := FFrames.Add(VmStack.Count,VmStack,Func);

  newStackTop := VMStack.Count-ArgCount-1;

  FCurrentFrame.StackTop := NewStackTop;

  result := true;

end;

(*
 VM Stack

 [0][1][2][3][4][5][6][7][8][9]
 [a][b][c][d][e][f][g][h][i][j]

 (Stack top = 10)  - (ArgCount = 1) - 1


 frame stack
 [0][1][2][3][4][5][6][7][8]
 [a][b][c][d][e][f][g][h][i]

 this would put it at i


*)



function TVirtualMachine.CallValue(const callee : pValue; ArgCount : integer) : boolean;
var
  value : pValue;

begin
  result := false;

  if Callee.IsFunction then
  begin
    result := call(callee.Loxfunction, argCount);
    exit;
  end;

  if Callee.isNative then
  begin
     value := Callee.NativeFunction.Native(ArgCount,VmStack);
     VMStack.Push(value);
     result := true;
     exit;
  end;

end;


procedure TVirtualMachine.DoReturn;
var
  result : pValue;
  StackTop : Integer;
  Frame  : TCallFrame;

begin

    Result := PopStack;

    Frame :=  FCurrentFrame;

    FFrames.Remove(FCurrentFrame);

    FCurrentFrame := FFrames.Frame;

    if FFrames.Count = 0 then
    begin
      VMStack.Pop;
      exit;
    end;

    VMStack.StackTop := Frame.StackTop;

    VMStack.Push(result);

    Frame.free;
end;


procedure TVirtualMachine.DoCall;
var
  ArgCount : byte;
  callee : pValue;
begin

  ArgCount := FCurrentFrame.InstructionPointer.Next^; //read byte

  callee := VMStack.peek(ArgCount);

  if not callValue(Callee,ArgCount) then
  begin
    raise exception.create('failed to complete call value');
  end;

end;


procedure TVirtualMachine.Loop;
var
  a,b : pbyte;
  offset : integer;
begin
   a := FCurrentFrame.InstructionPointer.Next;
   b := FCurrentFrame.InstructionPointer.Next;
   offset := a^ shl 8 + b^;
   assert(FCurrentFrame.InstructionPointer.Move(FCurrentFrame.InstructionPointer.Index - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.Jump;
var
  a,b : pbyte;
  offset : integer;
begin
  a := FCurrentFrame.InstructionPointer.Next;
  b := FCurrentFrame.InstructionPointer.Next;
  offset := a^ shl 8 + b^;
  assert(FCurrentFrame.InstructionPointer.Move(FCurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump offset');
end;

procedure TVirtualMachine.JumpFalse;
var
  a,b : pbyte;
  offset : integer;
begin
   
   a := FCurrentFrame.InstructionPointer.Next;
   b := FCurrentFrame.InstructionPointer.Next;
   assert(a <> nil, 'a is nil can''t jump');
   assert(b <> nil, 'b is nil can''t jump');
   offset := a^ shl 8 + b^;
   if (isFalsey(VMStack.peek(0))) then
   begin
     assert(FCurrentFrame.InstructionPointer.Move(FCurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.Negate;
var
  Result : pValue;
  R : pValue;
begin

  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.Pop;

    if (R.IsNumber) then
    begin
      Result := newNumber(- R.Number);
      VMStack.Push(Result);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
    end;

  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


//it's very important when working with the stack as it is pointer based,
// and what you might push back on is actually a global var in altered form.
//therefore, for now, always use a result, and pop off old vals
procedure TVirtualMachine.Divide;
var
  L,R, Result : pValue;
begin

  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.Pop;
    Assert(R.Number <> 0); //divide by zero exceptions.
    L := VMStack.Pop;

    Result := NewNumber(L.Number / R.Number);

    VMStack.Push(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

Function TVirtualMachine.isFalsey(value : pValue) : Boolean;
begin
  result :=
    (Value.Kind = lxNull) OR
    ((Value.Kind = lxBoolean) and (Value.Boolean = false)) OR
    ((Value.Kind = lxNumber) and (Value.Number <= 0)) OR
    ((Value.IsString) and (lowercase(Value.ToString) = 'false'));
end;

{  C code...
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

procedure TVirtualMachine.NotEqual;
var
  result : pValue;

begin

  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_NOT), 'Current instruction is <> NOT');
  try
    result := NewBool(isFalsey(VMStack.pop));
    VMStack.Push(Result);
   // FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R, Result : pValue;
begin

  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := VMStack.Pop;
    L := VMStack.Pop;

    Result := NewBool(r.ToString = l.ToString);
    VMStack.Push(result);
  //  FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.DoTrue;
var
  value : pValue;
begin

  value := newBool(true);
  VMStack.Push(value);
end;


procedure TVirtualMachine.DoFalse;
var
  Value : pValue;
begin

  Value := NewBool(False);
  VMStack.Push(Value);
end;

procedure TVirtualMachine.DoNil;
var
  value : pValue;
begin

  new(Value);
  Value.Null := true;
  VMStack.Push(value);
end;





(*
        uint8_t slot = READ_BYTE();
        frame->slots[slot] = peek(0);
*)
procedure TVirtualMachine.OPSetLocal;
var
  index : Integer;

  value : pValue;
begin

  assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_Set_LOCAL), 'current instruction is not op define global');

  moveNext;

  index := FCurrentFrame.InstructionPointer.CurrentByte^ ;

  value :=  VMStack.Peek(0);

  FCurrentFrame.Value[index] := Value;

end;




procedure TVirtualMachine.OpGetLocal;
var
  index  : Integer;
  Value  : pValue;
  Count  : Integer;
begin

  assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_Get_LOCAL), 'current instruction is not op define global');

  moveNext;

  Index :=  FCurrentFrame.InstructionPointer.CurrentByte^ ;

  value := FCurrentFrame.Value[index];

  VMStack.Push(Value);  // push(frame->slots[slot]);

end;


//suppose we expect 3 values;
function Foo(const ArgCount: Integer;const Values : TValueStack): pValue;
var
  v1,v2,v3 : pValue;
begin
  v1 := values.Peek(2);
  v2 := values.Peek(1);
  v3 := Values.Peek(0);

  result := newString(v1.toString + v2.ToString + v3.ToString);
end;





procedure TVirtualMachine.RegisterNatives;
begin
   AddGlobal(newString('foo'),NewNative(foo));
   AddGlobal(newString('DateTime'),NewNative(DateTime));
   AddGlobal(newString('FileExists'),NewNative(FileExists));
   AddGlobal(newString('LoadFromFile'),NewNative(LoadStringFromFile));
end;

function TVirtualMachine.VMStack : TValueStack;
begin
  result := FFrames.Stack;
end;

procedure TVirtualMachine.DoGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   NameValue : pNameValue;

   value : pValue;
begin

  assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_Get_GLOBAL), 'current instruction is not op define global');

  MoveNext;

  constantIndex := FCurrentFrame.InstructionPointer.CurrentByte^;

  name := FCurrentFrame.InstructionPointer.global[constantIndex];

  NameValue := FGlobals.Find(name.tostring);

  Assert(NameValue <> nil, 'expected value does not exist in globals');

  VMStack.push(NameValue.value);

end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin

  assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;
  constantIndex := FCurrentFrame.InstructionPointer.CurrentByte^;

  name := FCurrentFrame.InstructionPointer.global[constantIndex];
  value := VMStack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  NameValue := FGlobals.Find(name.tostring);
  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');
  NameValue.Value := Value;
end;



procedure TVirtualMachine.DoPOP;
begin

  VMStack.pop;
end;


procedure TVirtualMachine.AddGlobal(const name : pvalue; const Value : pValue);
begin

  assert(assigned(FGlobals.AddNameValue(Name,value)), 'failed to add to hash table');
end;

procedure TVirtualMachine.ClearLog;
begin

end;

procedure TVirtualMachine.Log(
  const opCOde : TopCodes;
  const operand : integer;
  const value   : pValue);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode) + ' .INDEX :' +  inttostr(operand) + '. VALUE:' + Value.ToString);
end;



procedure TVirtualMachine.Log(Const opCOde : TopCodes; const operand : integer);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode) + ' = ' +  inttostr(operand));
end;

procedure TVirtualMachine.Log(Const value : String);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + Value);
end;

procedure TVirtualMachine.Log(Const opCOde : TopCodes);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode));
end;

procedure TVirtualMachine.MoveNext;
begin
  Assert(FCurrentFrame.InstructionPointer.Next <> nil,'Expected constant value following constant operation');
end;

function TVirtualMachine.PeekStack: pValue;
begin

  result := VMStack.Peek;
end;

function TVirtualMachine.PopStack : pValue;
begin

  result := VMStack.Pop;
end;

procedure TVirtualMachine.DoGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;

begin

  assert(FCurrentFrame.InstructionPointer.CurrentByte^ = byte(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := FCurrentFrame.InstructionPointer.CurrentByte^;
  name := FCurrentFrame.InstructionPointer.global[constantIndex];
  value := VMStack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  AddGlobal(name,value);
  popStack;

end;


(*
result := pop;
closeUpvalues(frame.slots);
Dec(vm.frameCount);

if vm.frameCount = 0 then
begin
  pop;
  Exit(INTERPRET_OK);
end;

vm.stackTop := frame.slots;
push(result);
frame := vm.frames[vm.frameCount - 1];
  *)


procedure TVirtualMachine.init(
  const LoxFunction : pLoxFunction;
  const results : TStrings;
  const Log     : TStrings);
var
  Value : pValue;
begin
  FLog := Log;

  FHalt    := false;

  assert(Assigned(results),'No way to display results as no string storage passed in');

  FResults := results;

  FGlobals.Init;

  RegisterNatives;

  FStackResults := TValueList.Create(true);

  FFrames := TCallFrames.create;

  vmStack.Push(newValueFromFunction(LoxFunction));

  call(loxfunction, 0);

end;


procedure TVirtualMachine.finalize;
begin
//  FCurrentFrame.Stack.Free;
  FFrames.free;
  FStackResults.free;
  FGlobals.finalize;
  //FCurrentFrame.free; ??
end;




end.
