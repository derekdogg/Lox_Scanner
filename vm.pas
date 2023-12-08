unit vm;

interface

uses
  sysutils,
  classes,
  LOXTypes,
  Table,
  values,
  opcodes,
  natives,
  AdapterCalls;

const
   Frames_Max = 128;
   Stack_Max = Frames_Max * 256;


type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  TVirtualMachine = record
  private
    
    FCall : integer;
    FRootFunction : pValue;
   // CurrentFrame : TCallFrame;
    FHalt    : boolean;
    FNatives : TNatives;
    FStack   : TStack;
    FFrames : TCallFrames;
   // FStackResults : TValueList;  //keep track of new values added to stack.For disposal later.
    FGlobals : TValuePairs;
    FResults : TStrings;
    FLog : TStrings;
    function CurrentFrame : TCallFrame;
    function InstructionPointer : TInstructionPointer;
    function CurrentOpCode : integer;
    function NextInstruction : integer;
    procedure ClearLog;
    procedure Log(Const value : String);overload;
    procedure Log(Const opcode : TopCodes);overload;
    procedure Log(Const opCOde : TopCodes; const operand : integer); overload;
    procedure Log(const opCOde : TopCodes;const operand : integer;const value   : pValue); overload;
    procedure Log(const OpCode : TOpCodes; const L,R : pValue);overload;
    procedure Halt;
    function VMStack : TStack;

    function PopStack : pValue;
    function PeekStack : pValue;overload;
    function PeekStack(const distance : integer) : pValue;overload;
    procedure PushStack(const value : pValue);
    procedure MoveNext;
    procedure OpConstant;
    Procedure DoAdd;
    Procedure OpSubtract;
    Procedure Divide;
    Procedure Multiply;
    Procedure Negate;
    procedure OPPrint;
    procedure Loop;
    procedure Equal;
    procedure OPGreater;
    procedure OPLess;
    procedure OPNotEqual;
    procedure OpTrue;
    procedure OpFalse;
    procedure OpNil;
    procedure OpDefineGlobal;
    procedure DoGetGlobal;
    procedure RegisterNatives;
    procedure DoSetGlobal;
    procedure OpPOP;
    procedure OpGetLocal;
    procedure OPSetLocal;
    procedure HandleRunTimeError(const E: Exception);
    procedure JumpFalse;
    procedure Jump;
    function Call(const Func : pLoxfunction; const ArgCount : integer) : boolean;
    procedure OpCall;
    procedure OpReturn;
    function CallValue(const callee : pValue; ArgCount : integer) : boolean;
    Function isFalsey(value : pValue) : Boolean;
    Procedure AddGlobal(
      const name : string;
      const Value : pValue;
      const ownValue : boolean);
    procedure OpBuildList;
    procedure OpIndexSubscriber;
    procedure OpStoreSubscriber;
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
  dialogs, addition, subtraction, valueManager;

function TVirtualMachine.Run : TInterpretResult;
begin

  clearLog;

  Result := INTERPRET_NONE;
  if InstructionPointer.Count = 0 then exit;
  while (FHalt = false) and (NextInstruction <> -1) do
  begin

    Case TOpCodes(CurrentOpCode) of

    OP_BUILD_LIST : begin
       OpBuildList;
    end;


    OP_INDEX_SUBSCR : begin
       OpIndexSubscriber;
    end;

    OP_STORE_SUBSCR : begin
      OpStoreSubscriber;
    end;

      OP_CONSTANT : begin
         OpConstant;
      end;


      OP_DEFINE_GLOBAL: begin
          Log(OP_DEFINE_GLOBAL);
          OpDefineGlobal;
       end;

      OP_POP : Begin
         Log(OP_POP);
         OpPOP;
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
         OpNil;
      end;

      OP_TRUE : begin
         Log(OP_TRUE);
         OpTrue;
      end;

      OP_FALSE : begin
          Log(OP_FALSE);
         OpFalse;
      end;

      OP_GREATER : begin
          Log(OP_GREATER);
         OPgreater;
      end;

      OP_LESS : begin
          Log(OP_LESS);
         OPless;
      end;

      OP_EQUAL : begin
         Log(OP_EQUAL);
        equal;
      end;

      OP_NOT : begin
       Log(OP_NOT);
        OPNotEqual;
      end;


      OP_ADD : begin
         Log(OP_ADD);
        DoAdd;
      end;

      OP_SUBTRACT : begin
        OPSubtract;
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
         OPPrint;
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
         OpCall; //note here this will create a new stack frame, create a new IP. Something to be aware of.
      end;


      OP_Return :
      begin
         Log(OP_Return);
         OpReturn;
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

procedure TVirtualMachine.OpStoreSubscriber;
var
  item, Index, ListValue : pValue;
begin
   item   := PopStack;
   Index  := PopStack;
   ListValue := PopStack;
   Listvalue.List.Items[round(index.Number)] := Item;
   PushStack(Item);
end;

procedure TVirtualMachine.OpIndexSubscriber;
var
  indexValue, listValue, result: pValue;
  list: pValue;
  index: Integer;
begin
  assert(CurrentOpCode = byte(OP_INDEX_SUBSCR));
  indexValue := PopStack;
  listValue := PopStack;
  index := round(indexValue.Number);
  result := Listvalue.List.Items[index];
  PushStack(result);
end;

procedure TVirtualMachine.OpBuildList;
var
   value : pValue;
   itemCount: integer;
   i : integer;
begin
   assert(CurrentOpCode = byte(OP_BUILD_LIST));
   MoveNext;
   itemCount := CurrentOpCode;
   value :=  BorrowChecker.newValueList('');

   // Add items to list
  // PushStack(value); // So list isn't swept by GC in appendToList - [to do!!]
   for  i:= itemCount downto 1 do
   begin
      value.List.Items.Add(PeekStack(i));
   end;

  // PopStack;

   // Pop items from stack
   while itemCount > 0 do
   begin
      PopStack;
      Dec(itemCount);
    end;

   PushStack(value);
end;


(*function TVirtualMachine.InstructionPointer : TInstructionPointer;
begin
  result := CurrentFrame.InstructionPointer;
end; *)

function TVirtualMachine.CurrentOpCode : integer;
begin
  result := CurrentFrame.InstructionPointer.Current;
end;

procedure TVirtualMachine.OpConstant;
var
  constantIndex : integer;
  value : pValue;
begin
   assert(CurrentOpCode = byte(OP_CONSTANT));

   MoveNext;

   constantIndex := CurrentOpCode;

   value := CurrentFrame.InstructionPointer.Constant[constantIndex];

   PushStack(value);

   Log(OP_CONSTANT,ConstantIndex,value);

end;


procedure TVirtualMachine.OPGreater;
var
  L,R,Result : pValue;
begin

  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(CurrentOpCode = byte(OP_GREATER));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    L := PopStack;

    Result := BorrowChecker.NewBool(l.Number > r.Number);
    PushStack(result);
   // FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.OPLess;
var
  L,R, Result : pValue;
begin

  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(CurrentOpCode = byte(OP_LESS));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    L := PopStack;
    Result := BorrowChecker.NewBool(l.Number < r.Number);
    PushStack(Result);
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
  Assert(CurrentOpCode = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    L := PopStack;

    result := TAddition.Add(L,R);
//    if assigned(result) then
    PushStack(result);

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

procedure TVirtualMachine.OPSubtract;
var
  Result, L,R : pValue;
begin
  Assert(CurrentOpCode = byte(OP_SUBTRACT));
  R := PopStack;
  L := PopStack;
  Log(OP_SUBTRACT,L,R);
  result := TSubtraction.Subtract(L,R);
  PushStack(result);
end;

procedure TVirtualMachine.Multiply;
var
  L,R : pValue;
  i : integer;
  s : string;

begin
(*
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(CurrentOpCode = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    L := PopStack;

    if (L.IsNumber) and (R.IsNumber) then
    begin

      L.Number := L.Number * R.Number;
      PushStack(L);
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
       PushStack(L);
     end;

     if (R.IsString) and (L.IsNumber) then
     begin
       s := '';
       for i := 0 to round(abs(L.Number))-1 do
       begin
         s := s + R.Str
       end;
       R.Str := s;
       PushStack(R);
     end;

     //code duplication

 except on E:exception do
     HandleRunTimeError(e);
  end; *)
end;

procedure TVirtualMachine.OPPrint;
var
  value : pValue;
begin

  Assert(CurrentOpCode = byte(OP_PRINT));
  //value := CurrentFrame.Stack.Pop;
  value := PopStack;

  FResults.Add(Value.ToString); //all this does is pass the string to a TStrings object
end;


procedure TVirtualMachine.PushStack(const value: pValue);
begin
  VMStack.Push(Value);
end;

function TVirtualMachine.CurrentFrame : TCallFrame;
begin
  result := FFrames.LastFrame;
end;

function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : integer) : boolean;
var
  prevOffset : integer;
  newStackTop : integer;

begin
  FCall := FCall + 1;

  result := false;

  if not (argCount = func.Arity) then raise exception.create('param mismatch');

  prevOffset :=  0;

  FFrames.Push(Func);

  newStackTop := VMStack.StackTop-ArgCount-1;

  CurrentFrame.StackTop := NewStackTop;

  result := true;
end;

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
     PushStack(value);
     result := true;
     exit;
  end;
end;

procedure TVirtualMachine.OpReturn;
var
  result : pValue;
  StackTop : Integer;
  Frame  : TCallFrame;

begin
    FCall := FCall - 1;

    Result := PopStack;

    Frame :=  CurrentFrame;

    //FFrames.Remove(CurrentFrame);
    FFrames.Pop;
    //Frame.Free;

    if FFrames.Count = 0 then
    begin
      PopStack;
      exit;
    end;

    VMStack.StackTop := Frame.StackTop;

    PushStack(result);


end;


procedure TVirtualMachine.OpCall;
var
  ArgCount : byte;
  callee : pValue;
begin

  ArgCount := NextInstruction; //read byte

  callee := peekStack(ArgCount);

  if not callValue(Callee,ArgCount) then
  begin
    raise exception.create('failed to complete call value');
  end;

end;

function TVirtualMachine.NextInstruction : integer;
begin
  result := CurrentFrame.InstructionPointer.Next;
end;

procedure TVirtualMachine.Loop;
var
  a,b : integer;
  offset : integer;
begin
   a := NextInstruction;
   b := NextInstruction;
   offset := a shl 8 + b;
   assert(CurrentFrame.InstructionPointer.Move(CurrentFrame.InstructionPointer.Index - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.Jump;
var
  a,b : integer;
  offset : integer;
begin
  a := NextInstruction;
  b := NextInstruction;
  offset := a shl 8 + b;
  assert(CurrentFrame.InstructionPointer.Move(CurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump offset');
end;

procedure TVirtualMachine.JumpFalse;
var
  a,b : integer;
  offset : integer;
begin
   a := NextInstruction;
   b := NextInstruction;
   offset := a shl 8 + b;
   if (isFalsey(PeekStack)) then
   begin
     assert(CurrentFrame.InstructionPointer.Move(CurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.Negate;
var
  Result : pValue;
  R : pValue;
begin

  Assert(CurrentOpCode = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;

    if (R.IsNumber) then
    begin
      Result := BorrowChecker.newNumber(- R.Number);
      PushStack(Result);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
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
  Assert(CurrentOpCode = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    Assert(R.Number <> 0); //divide by zero exceptions.
    L := PopStack;

    Result := BorrowChecker.NewNumber(L.Number / R.Number);

    PushStack(Result);
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



procedure TVirtualMachine.OPNotEqual;
var
  result : pValue;

begin

  Assert(CurrentOpCode = ord(OP_NOT), 'Current instruction is <> NOT');
  try
    result := BorrowChecker.NewBool(isFalsey(PopStack));
    PushStack(Result);
   // FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R, Result : pValue;
begin
  Assert(CurrentOpCode = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');

  try
    R := PopStack;
    L := PopStack;

    Result := BorrowChecker.NewBool(r.ToString = l.ToString);
    PushStack(result);

  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.OpTrue;
var
  value : pValue;
begin
  value := BorrowChecker.newBool(true);
  PushStack(value);
end;


procedure TVirtualMachine.OpFalse;
var
  Value : pValue;
begin
  Value := BorrowChecker.NewBool(False);
  PushStack(Value);
end;

procedure TVirtualMachine.OpNil;
var
  value : pValue;
begin
  Value := BorrowChecker.NewNil;
  PushStack(value);
end;

procedure TVirtualMachine.OPSetLocal;
var
  index : Integer;
  value : pValue;
begin

  assert(CurrentOpCode = byte(OP_Set_LOCAL), 'current instruction is not op define global');

  moveNext;

  index := CurrentOpCode ;

  value :=  PeekStack;

  CurrentFrame.Value[index] := Value;

end;




procedure TVirtualMachine.OpGetLocal;
var
  index  : Integer;
  Value  : pValue;
  Count  : Integer;
begin

  assert(CurrentOpCode = byte(OP_Get_LOCAL), 'current instruction is not op define global');

  moveNext;

  Index :=  CurrentOpCode;

  value := CurrentFrame.Value[index];

  PushStack(Value);  // push(frame->slots[slot]);

end;


//suppose we expect 3 values;
function Foo(const ArgCount: Integer;const Values : TStack): pValue;
var
  v1,v2,v3 : pValue;
begin
  v1 := values.Peek(2);
  v2 := values.Peek(1);
  v3 := Values.Peek(0);

  result := BorrowChecker.newString(rVM,v1.toString + v2.ToString + v3.ToString);
end;

procedure TVirtualMachine.Halt;
begin
  FHalt := true;
end;

procedure TVirtualMachine.RegisterNatives;
begin

   AddGlobal( ('foobar'),BorrowChecker.NewNative(foo), true);
   AddGlobal( ('DateTime'),BorrowChecker.NewNative(DateTime), true);
   AddGlobal( ('FileExists'),BorrowChecker.NewNative(FileExists), true);
   AddGlobal( ('LoadFromFile'),BorrowChecker.NewNative(LoadStringFromFile), true);
 
end;

function TVirtualMachine.VMStack : TStack;
begin
  result := FStack;//FFrames.Stack;
end;

procedure TVirtualMachine.DoGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   NameValue : pNameValue;

   value : pValue;
begin

  assert(CurrentOpCode = byte(OP_Get_GLOBAL), 'current instruction is not op define global');

  MoveNext;

  constantIndex := CurrentOpCode;

  name := CurrentFrame.InstructionPointer.global[constantIndex];

  NameValue := FGlobals.Find(name.tostring);

  Assert(NameValue <> nil, 'expected value does not exist in globals');

  PushStack(NameValue.value);

end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin

  assert(CurrentOpCode = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;
  constantIndex := CurrentOpCode;

  name := CurrentFrame.InstructionPointer.global[constantIndex];
  value := PeekStack;
  assert(name.IsString, 'name is not a string object');
  NameValue := FGlobals.Find(name.tostring);
  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');
  NameValue.Value := Value;
end;


procedure TVirtualMachine.OpPOP;
begin
  VMStack.pop;
end;


procedure TVirtualMachine.AddGlobal(
  const name : string;
  const Value : pValue;
  const ownValue : boolean);
begin
  assert(assigned(FGlobals.AddNameValue(Name,value, OwnValue)), 'failed to add to hash table');
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

procedure TVirtualMachine.Log(Const opCode : TopCodes);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode));
end;

procedure TVirtualMachine.MoveNext;
begin
  Assert(NextInstruction <> -1,'Expected constant value following constant operation');
end;

function TVirtualMachine.PeekStack: pValue;
begin
  result := VMStack.Peek;
end;

function TVirtualMachine.PeekStack(const distance: integer): pValue;
begin
  result := VMStack.Peek(Distance);
end;

function TVirtualMachine.PopStack : pValue;
begin
  result := VMStack.Pop;
end;

procedure TVirtualMachine.OpDefineGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;

begin
  assert(CurrentOpCode = ord(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := CurrentOpCode;
  name := CurrentFrame.InstructionPointer.global[constantIndex];
  value := PeekStack;
  assert(name.IsString, 'name is not a string object');
  AddGlobal(name.Tostring,value, false);
  popStack;
end;




procedure TVirtualMachine.init(
  const LoxFunction : pLoxFunction;
  const results : TStrings;
  const Log     : TStrings);
var
  Value : pValue;
begin
 

  FCall := 0;

  FStack := TStack.Create;
 
  FLog := Log;

  FHalt    := false;

  assert(Assigned(results),'No way to display results as no string storage passed in');

  FResults := results;

  FGlobals.Init;

  RegisterNatives;

 // FStackResults := TValueList.Create(true);

  FFrames := TCallFrames.create(FStack);

  FRootFunction := BorrowChecker.newValueFromFunction(LoxFunction);

  PushStack(FRootFunction);

  call(loxfunction, 0);

end;


function TVirtualMachine.InstructionPointer: TInstructionPointer;
begin
  result := CurrentFrame.InstructionPointer;
end;

procedure TVirtualMachine.finalize;
begin
   

   Showmessage('op call amount: ' + inttostr(FCall));

   FStack.Free;

   FGlobals.finalize;

   FFrames.free;

   dispose(FRootFunction);
end;




end.
