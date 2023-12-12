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

  TVirtualMachine = class
  private
    FOPCode : integer;
    MemStatus: TMemoryManagerState;
    FCall : integer;
    FRootFunction : pValueRecord;
    FCurrentFrame : TCallFrame;
    FHalt    : boolean;
    FNatives : TNatives;
    FStack   : TStack;
    FFrames : TCallFrames;

    FGlobals : TValuePairs;
    FResults : TStrings;
    FLog : TStrings;
    procedure Execute;
    procedure AssertCurrentOp(OpCode : TOpCodes);
    function CurrentFrame : TCallFrame;
    function InstructionPointer : TInstructionPointer;
    function CurrentOpCode : integer;
    function NextInstruction : integer;
    procedure ClearLog;
    procedure Log(Const value : String);overload;
    procedure Log(Const opcode : TopCodes);overload;
    procedure Log(Const opCOde : TopCodes; const operand : integer); overload;
    procedure Log(const opCOde : TopCodes;const operand : integer;const value   : pValueRecord); overload;
    procedure Log(const OpCode : TOpCodes; const L,R : pValueRecord);overload;
    procedure Halt;
    function VMStack : TStack;
    procedure PushFrame(const func : PLoxFunction);
    procedure  popFrame;
    function PopStack : pValueRecord;
    function PeekStack : pValueRecord;overload;
    function PeekStack(const distance : integer) : pValueRecord;overload;
    procedure PushStack(const value : pValueRecord);
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
    function CallValue(const callee : pValueRecord; ArgCount : integer) : boolean;
    Function isFalsey(value : pValueRecord) : Boolean;
    Procedure AddGlobal(
      const name : string;
      const Value : pValueRecord;
      const ownValue : boolean);
    procedure OpBuildList;
    procedure OpIndexSubscriber;
    procedure OpStoreSubscriber;
  public
//    function Result : TByteCode;
    function Run(const func : PLoxFunction) : TInterpretResult;

    constructor create(
       const results : TStrings;
       const Log     : TStrings);
     Destructor Destroy; override;

  end;

implementation

uses
  dialogs, addition, subtraction, valueManager;

procedure TVirtualMachine.Execute;
begin
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
        OPGetGlobal;
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
        OPequal;
      end;

      OP_NOT : begin
       Log(OP_NOT);
        OPNotEqual;
      end;


      OP_ADD : begin
         Log(OP_ADD);
        OPAdd;
      end;

      OP_SUBTRACT : begin
        OPSubtract;
      end;


      OP_DIVIDE : begin
         Log(OP_DIVIDE);
        OPdivide;
      end;

      OP_MULTIPLY : begin
         Log(OP_MULTIPLY);
         OPMultiply;
      end;

      OP_NEGATE : begin
         Log(OP_NEGATE);
         OpNegate;
      end;

      OP_PRINT  : begin
         Log(OP_PRINT);
         OPPrint;
      end;


      OP_JUMP_IF_FALSE:
      begin
         Log(OP_JUMP_IF_FALSE);
         OPJumpFalse;
      end;


      OP_JUMP:
      begin
         Log(OP_JUMP);
         OpJump;
      end;


       OP_LOOP:
       begin
         Log(OP_LOOP);
         Oploop;
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


end;

function TVirtualMachine.Run(const func : PLoxFunction) : TInterpretResult;
begin
  FRootFunction := BorrowChecker.newValueFromFunction(Func);

  PushStack(FRootFunction);

  call(Func, 0);

  clearLog;

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
  item, Index, ListValue : pValueRecord;

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
  indexValue, listValue, result: pValueRecord;
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
   NewList : pValueRecord;
   Item : pValueRecord;
   List : pLoxList;
   itemCount: integer;
   i : integer;
begin
   assert(CurrentOpCode = byte(OP_BUILD_LIST));
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

   PushStack(NewList);
end;


(*function TVirtualMachine.InstructionPointer : TInstructionPointer;
begin
  result := CurrentFrame.InstructionPointer;
end; *)

function TVirtualMachine.CurrentOpCode : integer;
begin
  result := FCurrentFrame.InstructionPointer.Current;
end;

procedure TVirtualMachine.OpConstant;
var
  constantIndex : integer;
  value : pValueRecord;
begin
   AssertCurrentOp(OP_CONSTANT);

   MoveNext;

   constantIndex := CurrentOpCode;

   value := FCurrentFrame.InstructionPointer.Constant[constantIndex];

   PushStack(value);

   Log(OP_CONSTANT,ConstantIndex,value);

end;  


procedure TVirtualMachine.OPGreater;
var
  L,R,Result : pValueRecord;
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
  L,R, Result : pValueRecord;
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
  L,R : pValueRecord;
  Result : pValueRecord;
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
 
procedure TVirtualMachine.Log(
  const OpCode : TOpCodes; const L,R : pValueRecord);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode) + '. LEFT : ' + GetString(L) + ' RIGHT:' + GetString(R));

end;

procedure TVirtualMachine.OPSubtract;
var
  Result, L,R : pValueRecord;
begin
  Assert(CurrentOpCode = byte(OP_SUBTRACT));
  R := PopStack;
  L := PopStack;
  Log(OP_SUBTRACT,L,R);
  result := TSubtraction.Subtract(L,R);
  assert(Result <> nil, 'result of subtraction was nil indicating failure');
  PushStack(result);
end;

procedure TVirtualMachine.OpMultiply;
var
  L,R : pValueRecord;
  Result : pValueRecord;
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
  value : pValueRecord;
begin
  AssertCurrentOp(OP_PRINT);
  //value := CurrentFrame.Stack.Pop;
  value := PopStack;

  FResults.Add(GetString(Value)); //all this does is pass the string to a TStrings object
end;


procedure TVirtualMachine.PushStack(const value: pValueRecord);
begin
  VMStack.Push(Value);
end;

function TVirtualMachine.CurrentFrame : TCallFrame;
begin
  result := FCurrentFrame;
end;


procedure TVirtualMachine.PushFrame(const func : PLoxFunction);
var
  InstructionPointer : TInstructionPointer;

begin
  //we could probably just keep reususing the IP as long as you return to the current OPCode (before call here) after return.
  InstructionPointer := TInstructionPointer.create;
  InstructionPointer.Func := Func;
  FCurrentFrame := TCallFrame.Create(InstructionPointer,FStack);

  FFrames.Push(FCurrentFrame);

end;

procedure TVirtualMachine.PopFrame;
var
  IP : TInstructionPointer;
  Frame : TCallFrame;
begin

  Frame := FFrames.Pop; //pop off the current frame

  //free the current frame, make the new current frame the last item now on the stack.
  IP := Frame.InstructionPointer;
  IP.free;

  FreeAndNil(FCurrentFrame);

  FCurrentFrame := FFrames.Peek;
end;




function TVirtualMachine.CallValue(const callee : pValueRecord; ArgCount : integer) : boolean;
var
  value : pValueRecord;
  fn    : pLoxFunction;
begin
  value := nil;
  fn    := nil;

  result := false;

  if GetIsFunction(Callee) then
  begin
    fn := GetFunction(callee);
    result := call(fn, argCount);
    exit;
  end;

  if GetIsNative(Callee) then
  begin
     value := GetNative(Callee).Native(ArgCount,VmStack);
     PushStack(value);
     result := true;
     exit;
  end;
end;



function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : integer) : boolean;

var
   newStackTop : integer;

begin
  FCall := FCall + 1;

  result := false;

  if not (argCount = func.Arity) then raise exception.create('param mismatch');

  PushFrame(func);

  newStackTop := VMStack.StackTop-ArgCount-1;

  FCurrentFrame.StackTop := newStacktop;

  result := true;
end;

procedure TVirtualMachine.OpReturn;
var
  result : pValueRecord;
  ip : TInstructionPointer;
begin
    AssertCurrentOp(OP_RETURN);

    FCall := FCall - 1;

    Result := PopStack;

    VMStack.StackTop := FCurrentFrame.StackTop;

    PushStack(result);

    PopFrame;
end;


procedure TVirtualMachine.OpCall;
var
  ArgCount : byte;
  callee : pValueRecord;
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
  result := FCurrentFrame.InstructionPointer.Next;
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
   idx := FCurrentFrame.InstructionPointer.Index;
   offset := a shl 8 + b;
   assert(FCurrentFrame.InstructionPointer.Move(idx - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.OPJump;
var
  a,b : integer;
  offset : integer;
begin
  a := NextInstruction;
  b := NextInstruction;
  offset := a shl 8 + b;
  assert(FCurrentFrame.InstructionPointer.Move(FCurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump offset');
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
     assert(FCurrentFrame.InstructionPointer.Move(FCurrentFrame.InstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.OpNegate;
var
  Result : pValueRecord;
  R : pValueRecord;
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
  L,R, Result : pValueRecord;
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

Function TVirtualMachine.isFalsey(value : pValueRecord) : Boolean;
begin
  result :=
    (Value.Kind = lxNull) OR
    ((Value.Kind = lxBoolean) and (GetBoolean(Value) = false)) OR
    ((Value.Kind = lxNumber) and (GetNumber(Value) <= 0)) OR
    ((GetIsString(Value)) and (lowercase(GetString(Value)) = 'false'));
end;



procedure TVirtualMachine.OPNotEqual;
var
  result : pValueRecord;

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
  L,R, Result : pValueRecord;
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
  value : pValueRecord;
begin
  value := BorrowChecker.newBool(true);
  PushStack(value);
end;


procedure TVirtualMachine.OpFalse;
var
  Value : pValueRecord;
begin
  Value := BorrowChecker.NewBool(False);
  PushStack(Value);
end;

procedure TVirtualMachine.OpNil;
var
  value : pValueRecord;
begin
  Value := BorrowChecker.NewNil;
  PushStack(value);
end;

procedure TVirtualMachine.OPSetLocal;
var
  index : Integer;
  value : pValueRecord;
begin

  assert(CurrentOpCode = byte(OP_Set_LOCAL), 'current instruction is not op define global');

  moveNext;

  index := CurrentOpCode;

  value :=  PeekStack;

  FCurrentFrame.Value[index] := Value;

end;




procedure TVirtualMachine.OpGetLocal;
var
  index  : Integer;
  Value  : pValueRecord;
  Count  : Integer;
begin

  assert(CurrentOpCode = byte(OP_Get_LOCAL), 'current instruction is not op define global');

  moveNext;

  Index :=  CurrentOpCode;

  value := FCurrentFrame.Value[index];

  PushStack(Value);  // push(frame->slots[slot]);

end;


//suppose we expect 3 values;
function Foo(const ArgCount: Integer;const Values : TStack): pValueRecord;
var
  v1,v2,v3 : pValueRecord;
begin
  v1 := values.Peek(2);
  v2 := values.Peek(1);
  v3 := Values.Peek(0);

  result := BorrowChecker.newString(rVM,GetString(v1) + GetString(v2) + GetString(v3));
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

procedure TVirtualMachine.OPGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValueRecord;
   NameValue : pNameValue;

   value : pValueRecord;
begin

  assert(CurrentOpCode = byte(OP_Get_GLOBAL), 'current instruction is not op define global');

  MoveNext;

  constantIndex := CurrentOpCode;

  name := FCurrentFrame.InstructionPointer.constant[constantIndex];

  NameValue := FGlobals.Find(GetString(name));

  Assert(NameValue <> nil, 'expected value does not exist in globals');

  PushStack(NameValue.value);

end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : pValueRecord;
   value  : pValueRecord;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin

  assert(CurrentOpCode = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;

  constantIndex := CurrentOpCode;

  name := FCurrentFrame.InstructionPointer.Constant[constantIndex];

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
  const Value : pValueRecord;
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
  const value   : pValueRecord);
begin
  if not assigned(FLog) then exit;
  FLog.Add(inttostr(FLog.Count) + '.' + OpCodeToStr(opCode) + ' .INDEX :' +  inttostr(operand) + '. VALUE:' + GetString(Value));
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

function TVirtualMachine.PeekStack: pValueRecord;
begin
  Result := VMStack.Peek;
end;

function TVirtualMachine.PeekStack(const distance: integer): pValueRecord;
begin
  Result := VMStack.Peek(Distance);
end;

function TVirtualMachine.PopStack : pValueRecord;
begin
  Result := VMStack.Pop;
end;

procedure TVirtualMachine.OpDefineGlobal;
var
   ConstantIndex : integer;
   Name   : pValueRecord;
   value  : pValueRecord;

begin
  assert(CurrentOpCode = ord(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := CurrentOpCode;
  name := FCurrentFrame.InstructionPointer.constant[constantIndex];
  value := PeekStack;
  assert(GetIsString(name), 'name is not a string object');
  AddGlobal(GetString(name),value, false);
  popStack;
end;




Constructor TVirtualMachine.Create(
  const results : TStrings;
  const Log     : TStrings);
var
  Value : pValueRecord;
begin

  FCall := 0;

  FStack := TStack.Create;
 
  FLog := Log;

  FHalt    := false;

  assert(Assigned(results),'No way to display results as no string storage passed in');

  FResults := results;

  FGlobals.Init;

  RegisterNatives;

  FFrames := TCallFrames.create;


end;


function TVirtualMachine.InstructionPointer: TInstructionPointer;
begin
  result := FCurrentFrame.InstructionPointer;
end;

destructor TVirtualMachine.destroy;
begin

   FStack.Free;

   FGlobals.finalize;

   FFrames.free;

   dispose(FRootFunction); //dispose the wraper around the current function.
end;




end.
