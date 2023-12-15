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




type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);

  TVirtualMachine = class
  private
    FOnStackPush : TOnStackPush;
    FOnStackPop  : TOnStackPop;

    FOPCode : integer;
    MemStatus: TMemoryManagerState;
    FCall : integer;
    FRootFunction : TValueRecord;

    FHalt    : boolean;
    FNatives : TNatives;

    FStack   : TStack;
    FFrames : TCallFrames;

    FGlobals : TValuePairs;
    FResults : TStrings;

    procedure Execute;
    procedure AssertCurrentOp(OpCode : TOpCodes);
    function CurrentFrame : TCallFrame;
    function InstructionPointer : TInstructionPointer;
    function CurrentOpCode : integer;
    function NextInstruction : integer;

    procedure Halt;
    function VMStack : TStack;
    procedure PushFrame(
      const func : PLoxFunction;
      const ArgCount : integer);
    procedure  popFrame;
    function PopStack : TValueRecord;
    function PeekStack : TValueRecord;overload;
    function PeekStack(const distance : integer) : TValueRecord;overload;
    procedure PushStack(const value : TValueRecord);
    procedure MoveNext;
    procedure OpConstant;
    Procedure OPAdd;
    Procedure OpSubtract;
    Procedure OPDivide;
    Procedure OPMultiply;
    Procedure OPNegate;
    procedure OPPrint;
    procedure OpLoop;
    procedure OPEqual;
    procedure OPGreater;
    procedure OPLess;
    procedure OPNotEqual;
    procedure OpTrue;
    procedure OpFalse;
    procedure OpNil;
    procedure OpDefineGlobal;
    procedure OPGETGLOBAL;
    procedure RegisterNatives;
    procedure DoSetGlobal;
    procedure OpPOP;
    procedure OpGetLocal;
    procedure OPSetLocal;
    procedure HandleRunTimeError(const E: Exception);
    procedure OPJumpFalse;
    procedure OpJump;
    function Call(const Func : pLoxfunction; const ArgCount : integer) : boolean;
    procedure OpCall;
    procedure OpReturn;
    function CallValue(const callee : TValueRecord; ArgCount : integer) : boolean;
    Function isFalsey(value : TValueRecord) : Boolean;
    Procedure AddGlobal(
      const name : string;
      const Value : TValueRecord;
      const ownValue : boolean);
    procedure OpBuildList;
    procedure OpIndexSubscriber;
    procedure OpStoreSubscriber;

    procedure CaptureStackPush(Const stack : TStack);
    procedure CaptureStackPop(Const stack : TStack);
  public
//    function Result : TByteCode;
    function Run(const func : PLoxFunction) : TInterpretResult;

    constructor create(
       const results : TStrings);
     Destructor Destroy; override;
    property OnPush : TOnStackPush read FOnStackPush write FOnStackPush;
    property OnPop : TOnStackPop read FOnStackPop write FOnStackPop;
  end;




implementation

uses
  dialogs, addition, subtraction, valueManager;

procedure TVirtualMachine.Execute;
begin
  while (FHalt = false) and
        (CurrentFrame <> nil) and
        (NextInstruction <> -1) do
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

          OpDefineGlobal;
       end;

      OP_POP : Begin

         OpPOP;
      end;


      OP_SET_GLOBAL:
      begin

        DoSetGlobal;
      end;



      OP_GET_GLOBAL:
      begin

        OPGetGlobal;
      end;


      OP_GET_LOCAL:
      begin
        OpGetLocal;
      end;

      OP_SET_LOCAL:
      begin

        OPSetLocal;
      end;


      OP_Nil  : begin

         OpNil;
      end;

      OP_TRUE : begin

         OpTrue;
      end;

      OP_FALSE : begin

         OpFalse;
      end;

      OP_GREATER : begin

         OPgreater;
      end;

      OP_LESS : begin

         OPless;
      end;

      OP_EQUAL : begin

        OPequal;
      end;

      OP_NOT : begin
       
        OPNotEqual;
      end;


      OP_ADD : begin

        OPAdd;
      end;

      OP_SUBTRACT : begin
        OPSubtract;
      end;


      OP_DIVIDE : begin

        OPdivide;
      end;

      OP_MULTIPLY : begin

         OPMultiply;
      end;

      OP_NEGATE : begin

         OpNegate;
      end;

      OP_PRINT  : begin

         OPPrint;
      end;


      OP_JUMP_IF_FALSE:
      begin

         OPJumpFalse;
      end;


      OP_JUMP:
      begin

         OpJump;
      end;


       OP_LOOP:
       begin

         Oploop;
       end;

      OP_CALL :
      begin
         OpCall;
      end;


      OP_Return :
      begin
          
         OpReturn;
      end;

    end;

  end;


end;

function TVirtualMachine.Run(const func : PLoxFunction) : TInterpretResult;
begin
  FRootFunction := BorrowChecker.newValueFromFunction(rVM,Func);

  PushStack(FRootFunction);

  call(Func, 0);

  Result := INTERPRET_NONE;

  if InstructionPointer.Count = 0 then exit;

  Execute;

  PopStack;

  popFrame;

  Result := INTERPRET_OK;
end;


procedure TVirtualMachine.HandleRunTimeError(const E : Exception);
begin
  showmessage(E.message);
  FHalt := true;
end;

procedure TVirtualMachine.OpStoreSubscriber;
var
  item, Index, ListValue : TValueRecord;

  List : pLoxList;
begin
   item   := PopStack;
   Index  := PopStack;
   ListValue := PopStack;
   //  Listvalue.List.Items[round(index.Number)] := Item;
   List := GetList(ListValue);
   List.Items[round(index.Number)] := Item;
   PushStack(Item);
end;

procedure TVirtualMachine.OpIndexSubscriber;
var
  indexValue, listValue, result: TValueRecord;
  index: Integer;
  List : pLoxList;
begin
  assert(CurrentOpCode = byte(OP_INDEX_SUBSCR));
  indexValue := PopStack;
  listValue := PopStack;
  index := round(indexValue.Number);
  List := GetList(ListValue);
  result := List.Items[index];
  PushStack(result);
end;

procedure TVirtualMachine.OpBuildList;
var
   NewList : TValueRecord;
   Item : TValueRecord;
   List : pLoxList;
   itemCount: integer;
   i : integer;
begin
  (* assert(CurrentOpCode = byte(OP_BUILD_LIST));
   MoveNext;
   itemCount := CurrentOpCode;
   NewList :=  BorrowChecker.newValueList(true,'');

   // Add items to list
  // PushStack(value); // So list isn't swept by GC in appendToList - [to do!!]

   List := GetList(NewList);
   for  i:= itemCount downto 1 do
   begin
      Item := PeekStack(i-1);
      List.Items.Add(Item);
   end;

  // PopStack;

   // Pop items from stack
   while itemCount > 0 do
   begin
      PopStack;
      Dec(itemCount);
    end;

   PushStack(NewList);   *)
end;


(*function TVirtualMachine.InstructionPointer : TInstructionPointer;
begin
  result := CurrentFrame.InstructionPointer;
end; *)

function TVirtualMachine.CurrentOpCode : integer;
begin
  result := FFrames.peek.InstructionPointer.Current;
end;

procedure TVirtualMachine.OpConstant;
var
  constantIndex : integer;
  value : TValueRecord;
begin
   AssertCurrentOp(OP_CONSTANT);

   MoveNext;

   constantIndex := CurrentOpCode;

   value := FFrames.peek.InstructionPointer.Constant[constantIndex];

   PushStack(value);

end;  


procedure TVirtualMachine.OPGreater;
var
  L,R,Result : TValueRecord;
begin
  AssertCurrentOp(OP_Greater);

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
  L,R, Result : TValueRecord;
begin
  AssertCurrentOp(OP_LESS);
  try
    R := PopStack;
    L := PopStack;
    Result := BorrowChecker.NewBool(l.Number < r.Number);
    PushStack(Result);

 except on E:exception do
    HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.AssertCurrentOp(OpCode : TOpCodes);
begin
  Assert(CurrentOpCode = ord(OpCode),'Problem with current op code not equal to expected');
end;


procedure TVirtualMachine.OPAdd;
var
  L,R : TValueRecord;
  Result : TValueRecord;
begin
  AssertCurrentOp(OP_ADD);
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
 

procedure TVirtualMachine.OPSubtract;
var
  L,R : TValueRecord;
begin
  Assert(CurrentOpCode = byte(OP_SUBTRACT));
  R := PopStack;
  L := PopStack;

  L := TSubtraction.Subtract(L,R);
  //assert(Result <> nil, 'result of subtraction was nil indicating failure');
  PushStack(L);
end;

procedure TVirtualMachine.OpMultiply;
var
  L,R : TValueRecord;
  Result : TValueRecord;
  i : integer;
  s : string;

begin

  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(CurrentOpCode = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    L := PopStack;

    if getIsNumber(L) and getIsNumber(R) then
    begin

      result := BorrowChecker.NewNumber(GetNumber(L) * GetNumber(R));
      PushStack(Result);
      exit;
    end;


 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.OPPrint;
var
  value : TValueRecord;
begin
  AssertCurrentOp(OP_PRINT);
  //value := CurrentFrame.Stack.Pop;
  value := PopStack;

  FResults.Add(GetString(Value)); //all this does is pass the string to a TStrings object
end;


procedure TVirtualMachine.PushStack(const value: TValueRecord);
begin
  VMStack.Push(Value);

end;

function TVirtualMachine.CurrentFrame : TCallFrame;
begin
  result := FFrames.peek;
end;


procedure TVirtualMachine.PushFrame(
  const func : PLoxFunction;
  const ArgCount : integer) ;
var
  InstructionPointer : TInstructionPointer;
  Frame : TCallFrame;
begin
  //we could probably just keep reususing the IP as long as you return to the current OPCode (before call here) after return.
  InstructionPointer := TInstructionPointer.create;
  InstructionPointer.Func := Func;

  Frame := TCallFrame.Create(InstructionPointer,FStack);
  Frame.StackTop :=  VMStack.StackTop-ArgCount-1;

  FFrames.Push(Frame);
end;

procedure TVirtualMachine.PopFrame;
var
  IP : TInstructionPointer;
  Frame : TCallFrame;
begin
  Frame := FFrames.Pop; //pop off the current frame
  IP := Frame.InstructionPointer;
  IP.free;
  Frame.Free;
end;




function TVirtualMachine.CallValue(const callee : TValueRecord; ArgCount : integer) : boolean;
var
  value : TValueRecord;
  fn    : pLoxFunction;
begin
//  value := nil;
  fn    := nil;

  result := false;

  if GetIsFunction(Callee) then
  begin
    fn := GetFunction(callee);
    result := call(fn, argCount);
    exit;
  end;

(*  if GetIsNative(Callee) then
  begin
     value := GetNative(Callee).Native(ArgCount,VmStack);
     PushStack(value);
     result := true;
     exit;
  end; *)
end;



procedure TVirtualMachine.CaptureStackPop(const stack: TStack);
begin
  if assigned(FOnStackPop) then FOnStackPop(stack);
end;

procedure TVirtualMachine.CaptureStackPush(const stack: TStack);
begin
   if assigned(FOnStackPush) then FOnStackPush(stack);
end;

function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : integer) : boolean;

begin

  FCall := FCall + 1;

  result := false;

  if not (argCount = func.Arity) then raise exception.create('param mismatch');

  PushFrame(func,ArgCount);

  result := true;
end;

procedure TVirtualMachine.OpReturn;
var
  result : TValueRecord;
  ip : TInstructionPointer;
  f : TCallFrame;
begin
    AssertCurrentOp(OP_RETURN);

    FCall := FCall - 1;

    Result := PopStack;

    VMStack.StackTop := FFrames.Peek.StackTop;

//    VMStack.DecreaseCapacity; //note 

    PushStack(result);

    PopFrame;
end;


procedure TVirtualMachine.OpCall;
var
  ArgCount : byte;
  callee : TValueRecord;
begin
  AssertCurrentOp(OP_CALL);

  ArgCount := NextInstruction;

  callee := peekStack(ArgCount);

  if not callValue(Callee,ArgCount) then
  begin
    raise exception.create('failed to complete call value');
  end;

end;

function TVirtualMachine.NextInstruction : integer;
begin
  result := FFrames.Peek.InstructionPointer.Next;
end;

procedure TVirtualMachine.OPLoop;
var
  a,b : integer;
  offset : integer;
  idx : integer;
begin
   AssertCurrentOp(OP_LOOP);
   a := NextInstruction;
   b := NextInstruction;
   idx := FFrames.Peek.InstructionPointer.Index;
   offset := a shl 8 + b;
   assert(FFrames.Peek.InstructionPointer.Move(idx - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.OPJump;
var
  a,b : integer;
  offset : integer;
begin
  a := NextInstruction;
  b := NextInstruction;
  offset := a shl 8 + b;
  assert(FFrames.Peek.InstructionPointer.Move(FFrames.Peek.InstructionPointer.Index + offset) = true, 'failed to move to jump offset');
end;

procedure TVirtualMachine.OpJumpFalse;
var
  a,b : integer;
  offset : integer;
begin
   a := NextInstruction;
   b := NextInstruction;
   offset := a shl 8 + b;
   if (isFalsey(PeekStack)) then
   begin
     assert(FFrames.Peek.InstructionPointer.Move(FFrames.Peek.InstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.OpNegate;
var
  Result : TValueRecord;
  R : TValueRecord;
begin

  Assert(CurrentOpCode = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;

    if (GetIsNumber(R)) then
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
procedure TVirtualMachine.OpDivide;
var
  L,R, Result : TValueRecord;
begin

  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(CurrentOpCode = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := PopStack;
    Assert(GetNumber(R) <> 0); //divide by zero exceptions.
    L := PopStack;

    Result := BorrowChecker.NewNumber(GetNumber(L) / GetNumber(R));

    PushStack(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

Function TVirtualMachine.isFalsey(value : TValueRecord) : Boolean;
begin
  result :=
    (Value.Kind = lxNull) OR
    ((Value.Kind = lxBoolean) and (GetBoolean(Value) = false)) OR
    ((Value.Kind = lxNumber) and (GetNumber(Value) <= 0)) OR
    ((GetIsString(Value)) and (lowercase(GetString(Value)) = 'false'));
end;



procedure TVirtualMachine.OPNotEqual;
var
  result : TValueRecord;

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

procedure TVirtualMachine.OPEqual;
var
  L,R, Result : TValueRecord;
begin
  Assert(CurrentOpCode = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');

  try
    R := PopStack;
    L := PopStack;

    Result := BorrowChecker.NewBool(GetString(r) = GetString(l));
    PushStack(result);

  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.OpTrue;
var
  value : TValueRecord;
begin
  value := BorrowChecker.newBool(true);
  PushStack(value);
end;


procedure TVirtualMachine.OpFalse;
var
  Value : TValueRecord;
begin
  Value := BorrowChecker.NewBool(False);
  PushStack(Value);
end;

procedure TVirtualMachine.OpNil;
var
  value : TValueRecord;
begin
  Value := BorrowChecker.NewNil;
  PushStack(value);
end;

procedure TVirtualMachine.OPSetLocal;
var
  index : Integer;
  value : TValueRecord;
begin

  assert(CurrentOpCode = byte(OP_Set_LOCAL), 'current instruction is not op define global');

  moveNext;

  index := CurrentOpCode;

  value :=  PeekStack;

  FFrames.Peek.Value[index] := Value;

end;




procedure TVirtualMachine.OpGetLocal;
var
  index  : Integer;
  Value  : TValueRecord;
  Count  : Integer;
begin

  assert(CurrentOpCode = byte(OP_Get_LOCAL), 'current instruction is not op define global');

  moveNext;

  Index :=  CurrentOpCode;

  value := FFrames.Peek.Value[index];

  PushStack(Value);  // push(frame->slots[slot]);

end;


//suppose we expect 3 values;
function Foo(const ArgCount: Integer;const Values : TStack): TValueRecord;
var
  v1,v2,v3 : TValueRecord;
begin
(*  v1 := values.Peek(2);
  v2 := values.Peek(1);
  v3 := Values.Peek(0);

  result := BorrowChecker.newString(rVM,GetString(v1) + GetString(v2) + GetString(v3)); *)
end;

procedure TVirtualMachine.Halt;
begin
  FHalt := true;
end;

procedure TVirtualMachine.RegisterNatives;
begin

(*   AddGlobal( ('foobar'),BorrowChecker.NewNative(foo), true);
   AddGlobal( ('DateTime'),BorrowChecker.NewNative(DateTime), true);
   AddGlobal( ('FileExists'),BorrowChecker.NewNative(FileExists), true);
   AddGlobal( ('LoadFromFile'),BorrowChecker.NewNative(LoadStringFromFile), true);
  *)
end;

function TVirtualMachine.VMStack : TStack;
begin
  result := FStack;//FFrames.Stack;
end;

procedure TVirtualMachine.OPGetGlobal;
var
   ConstantIndex : integer;
   Name   : TValueRecord;
   NameValue : pNameValue;

   value : TValueRecord;
begin

  assert(CurrentOpCode = byte(OP_Get_GLOBAL), 'current instruction is not op define global');

  MoveNext;

  constantIndex := CurrentOpCode;

  name := FFrames.Peek.InstructionPointer.constant[constantIndex];

  NameValue := FGlobals.Find(GetString(name));

  Assert(NameValue <> nil, 'expected value does not exist in globals');

  PushStack(NameValue.value);

end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : TValueRecord;
   value  : TValueRecord;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin

  assert(CurrentOpCode = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;

  constantIndex := CurrentOpCode;

  name := FFrames.Peek.InstructionPointer.Constant[constantIndex];

  value := PeekStack;

  assert(GetIsString(name), 'name is not a string object');

  NameValue := FGlobals.Find(GetString(name));

  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');

  NameValue.Value := Value;
end;


procedure TVirtualMachine.OpPOP;
begin
  VMStack.pop;
end;


procedure TVirtualMachine.AddGlobal(
  const name : string;
  const Value : TValueRecord;
  const ownValue : boolean);
begin
  assert(assigned(FGlobals.AddNameValue(Name,value, OwnValue)), 'failed to add to hash table');
end;

procedure TVirtualMachine.MoveNext;
begin
  Assert(NextInstruction <> -1,'Expected constant value following constant operation');
end;

function TVirtualMachine.PeekStack: TValueRecord;
begin
  Result := VMStack.Peek;
end;

function TVirtualMachine.PeekStack(const distance: integer): TValueRecord;
begin
  Result := VMStack.Peek(Distance);
end;

function TVirtualMachine.PopStack : TValueRecord;
begin
  Result := VMStack.Pop;

  //add to temp stack

  //FTempStack.Push(Result);
end;

procedure TVirtualMachine.OpDefineGlobal;
var
   ConstantIndex : integer;
   Name   : TValueRecord;
   value  : TValueRecord;

begin
  assert(CurrentOpCode = ord(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := CurrentOpCode;
  name := FFrames.Peek.InstructionPointer.constant[constantIndex];
  value := PeekStack;
  assert(GetIsString(name), 'name is not a string object');
  AddGlobal(GetString(name),value, false);
  popStack;
end;


Constructor TVirtualMachine.Create(
  const results : TStrings);
var
  Value : TValueRecord;
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');
  FResults := results;

  FCall := 0;

  FStack := TStack.Create;
  FStack.OnPush := CaptureStackPush;
  FStack.OnPop :=  CaptureStackPop;
 
  FHalt    := false;

  FGlobals.Init;

  RegisterNatives;

  FFrames := TCallFrames.create;

end;


function TVirtualMachine.InstructionPointer: TInstructionPointer;
begin
  result := FFrames.Peek.InstructionPointer;
end;

destructor TVirtualMachine.destroy;
begin

   FStack.Free;

   FGlobals.finalize;

   FFrames.free;

  
end;




end.
