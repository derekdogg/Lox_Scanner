unit NewVm;

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


  TFrame = record
    InstructionPointerIdx : integer;
    Fn         : pLoxFunction;
    StackTop   : integer;
  end;


  TFrameItems = array[Byte] of TFrame;


  (*TFrames = record
    Items : TFrameItems;
    StackTop : integer;
  end; *)

  TVirtualMachine = class
  private
    FNameValue    : pNameValue;
    FCurrentFrame : TFrame;
    FInstructionPointerIdx : Integer;
    FOpCode : Integer;
    FInstructionPointer : TInstructionPointer;
    FOnStackPush : TOnStackPush;
    FOnStackPop  : TOnStackPop;
    FRootFunction : TValueRecord;
    FHalt    : boolean;
    FStack   : TStack;
    FFrames : TFrameItems;
    FFrameStackTop : integer;
    FGlobals : TValuePairs;
    FResults : TStrings;

    procedure Execute;
    //function CurrentFrame : TCallFrame;
//    function InstructionPointer : TInstructionPointer;
    function CurrentOpCode : integer;
    function NextInstruction : integer;
    procedure Halt;
    
    procedure PushFrame(
      const func : PLoxFunction;
      const ArgCount : integer);
    procedure  popFrame;
    function PopStack : TValueRecord;
    function PeekStack : TValueRecord;overload;
    function PeekStack(const distance : integer) : TValueRecord;overload;
    procedure PushStack(const value : TValueRecord);

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
    destructor Destroy; override;
    property OnPush : TOnStackPush read FOnStackPush write FOnStackPush;
    property OnPop : TOnStackPop read FOnStackPop write FOnStackPop;
  end;
 

implementation

uses
  dialogs, addition, subtraction, valueManager;

procedure TVirtualMachine.Execute;
begin
  while (FHalt = false) and
        (FFrameStackTop > 0) and
        (NextInstruction <> -1) do
  begin

    Case CurrentOpCode of

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

  if FInstructionPointer.Count = 0 then exit;

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



function TVirtualMachine.CurrentOpCode : integer;
begin
  result := FInstructionPointer.Current;
end;

procedure TVirtualMachine.OpConstant;
begin
   PushStack(FInstructionPointer.Constant[NextInstruction]);
end;


procedure TVirtualMachine.OPGreater;
var
  L,R : TValueRecord;
begin
    R := PopStack;
    L := PopStack;
    PushStack(BorrowChecker.NewBool(l.Number > r.Number));
end;



procedure TVirtualMachine.OPLess;
var
  L,R : TValueRecord;
begin
    R := PopStack;
    L := PopStack;

    PushStack(BorrowChecker.NewBool(l.Number < r.Number));

end;


procedure TVirtualMachine.OPAdd;
var
  L,R : TValueRecord;

begin
    R := PopStack;
    L := PopStack;

    PushStack(TAddition.Add(L,R));
end;


procedure TVirtualMachine.OPSubtract;
var
  L,R : TValueRecord;

begin

  R := PopStack;

  L := PopStack;

  PushStack(TSubtraction.Subtract(L,R));
end;

procedure TVirtualMachine.OpMultiply;
var
  L,R : TValueRecord;
  Result : TValueRecord;
  i : integer;
  s : string;

begin
    R := PopStack;
    L := PopStack;

    if getIsNumber(L) and getIsNumber(R) then
    begin

      result := BorrowChecker.NewNumber(GetNumber(L) * GetNumber(R));
      PushStack(Result);
      exit;
    end;

end;

procedure TVirtualMachine.OPPrint;
var
  value : TValueRecord;
begin
  value := PopStack;

  FResults.Add(GetString(Value));
end;


procedure TVirtualMachine.PushStack(const value: TValueRecord);
begin
  FStack.Push(Value);
end;

//dumb - try to break into two and then send through the actual pointer.
function TVirtualMachine.CallValue(const callee : TValueRecord; ArgCount : integer) : boolean;
begin
  result := false;
  if GetIsFunction(Callee) then
  begin
    result := call(GetFunction(callee), argCount);
    exit;
  end;

(*  if GetIsNative(Callee) then
  begin
     value := GetNative(Callee).Native(ArgCount,FStack);
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
  assert(Func <> nil,'no function has been initialized');

  if not (argCount = func.Arity) then
  begin
    raise exception.create('param mismatch');
  end;

  PushFrame(func,ArgCount);
 
  result := true;
end;

procedure TVirtualMachine.OpReturn;
var
  result : TValueRecord;
begin
    Result := PopStack; //result of the function

    FStack.StackTop :=  FCurrentFrame.StackTop;

    PushStack(result); //push result to new location in stack.

    PopFrame;
end;


procedure TVirtualMachine.OpCall;
var
  ArgCount : byte;
begin

  ArgCount := NextInstruction;

  if not callValue(peekStack(ArgCount),ArgCount) then
  begin
    raise exception.create('failed to complete call value'); //dumb - need better way to handle blow-ups
  end;

end;

function TVirtualMachine.NextInstruction : integer;
begin
  result := FInstructionPointer.Next;
end;

procedure TVirtualMachine.OPLoop;
begin
   assert(FInstructionPointer.Increment(-NextInstruction) = true, 'failed to move to loop offset'); //dumb - probably...smells
end;



procedure TVirtualMachine.OPJump;
begin
  assert(FInstructionPointer.Increment(NextInstruction) = true, 'failed to move to jump offset');
end;


procedure TVirtualMachine.OpJumpFalse;
begin
   if (isFalsey(PeekStack)) then
   begin
     assert(FInstructionPointer.Increment(NextInstruction) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.OpNegate;
var
  Result : TValueRecord;
  R : TValueRecord;
begin

    R := PopStack;

    if (GetIsNumber(R)) then
    begin
      Result := BorrowChecker.newNumber(- R.Number);
      PushStack(Result);
    end;

end;

procedure TVirtualMachine.OpDivide;
var
  L,R, Result : TValueRecord;
begin

    R := PopStack;
    Assert(GetNumber(R) <> 0); //divide by zero exceptions.
    L := PopStack;

    Result := BorrowChecker.NewNumber(GetNumber(L) / GetNumber(R));

    PushStack(Result);
  
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
begin
  PushStack(BorrowChecker.NewBool(isFalsey(PopStack)));
end;

procedure TVirtualMachine.OPEqual;
var
  L,R : TValueRecord;
begin
    R := PopStack;
    L := PopStack;
    PushStack(BorrowChecker.NewBool(GetString(r) = GetString(l)));
end;


procedure TVirtualMachine.OpTrue;
begin
  PushStack(BorrowChecker.newBool(true));
end;


procedure TVirtualMachine.OpFalse;
begin
  PushStack(BorrowChecker.NewBool(False));
end;

procedure TVirtualMachine.OpNil;
begin
  PushStack(BorrowChecker.NewNil);
end;


(*procedure TVirtualMachine.PushFrame(
  const func : PLoxFunction;
  const ArgCount : integer) ;
var
  Frame : TCallFrame;
begin
  Frame := TCallFrame.Create(FInstructionPointer.Index, Func,FStack,FStack.StackTop-ArgCount-1);
  FFrames.Push(Frame);

  FInstructionPointer.Func := Func;
  FInstructionPointer.Index := -1; //reset to point to the current funcs opcode starting at zero (after move next);
end; *)

procedure TVirtualMachine.PushFrame(
  const func : PLoxFunction;
  const ArgCount : integer) ;
begin
  assert(FFrameStackTop < high(Byte), 'Frame Stack pointer beyond limit');
  if FFrameStackTop > 0 then
  begin
    //bookmark where the instruction pointer is for the current callframe, to return to later.
    FFrames[FFrameStackTop-1].InstructionPointerIdx := FInstructionPointer.Index;
  end;

  FFrames[FFrameStackTop].Fn := Func;

  assert(FStack.StackTop-ArgCount-1 >= 0, 'not > 0');
  FFrames[FFrameStackTop].StackTop := FStack.StackTop-ArgCount-1;
  FFrameStackTop := FFrameStackTop + 1;
  FCurrentFrame := FFrames[FFrameStackTop-1];
  FInstructionPointer.Func := Func;
  FInstructionPointer.Index := -1; //reset to point to the current funcs opcode starting at zero (after move next);
end;
 

procedure TVirtualMachine.PopFrame;
var
  Frame : TCallFrame;
  InstructionPointerIdx : integer;
begin
  assert(FFrameStackTop > 0);

  FFrameStackTop := FFrameStackTop - 1;   //pop frame

  if FFrameStackTop > 0 then
  begin
    FCurrentFrame := FFrames[FFrameStackTop-1];
    FInstructionPointer.Func  :=  FCurrentFrame.Fn;
    FInstructionPointer.Index :=  FCurrentFrame.InstructionpointerIdx;
  end;

end;

procedure TVirtualMachine.OPSetLocal;
begin
  FStack[FCurrentFrame.StackTop + NextInstruction] := PeekStack;
end;


procedure TVirtualMachine.OpGetLocal;
begin
  PushStack(FStack[FCurrentFrame.StackTop + NextInstruction]);
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


procedure TVirtualMachine.OPGetGlobal;
var
   Name   : TValueRecord;
   NameValue : pNameValue;
begin

  name := FInstructionPointer.constant[NextInstruction];

  NameValue := FGlobals.Find(GetString(name));

  Assert(NameValue <> nil, 'expected value does not exist in globals');

  PushStack(NameValue.value);

end;


procedure TVirtualMachine.DoSetGlobal;
var
   Name   : TValueRecord;
   value  : TValueRecord;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin

  name := FInstructionPointer.Constant[NextInstruction];

  value := PeekStack;

  assert(GetIsString(name), 'name is not a string object');

  NameValue := FGlobals.Find(GetString(name));
 
  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');

  NameValue.Value := Value;
end;


procedure TVirtualMachine.OpPOP;
begin
  FStack.pop;
end;


procedure TVirtualMachine.AddGlobal(
  const name : string;
  const Value : TValueRecord;
  const ownValue : boolean);
begin
  assert(assigned(FGlobals.AddNameValue(Name,value, OwnValue)), 'failed to add to hash table');
end;


function TVirtualMachine.PeekStack: TValueRecord;
begin
  Result := FStack.Peek;
end;

function TVirtualMachine.PeekStack(const distance: integer): TValueRecord;
begin
  Result := FStack.Peek(Distance);
end;

function TVirtualMachine.PopStack : TValueRecord;
begin
  Result := FStack.Pop;

  //add to temp stack

  //FTempStack.Push(Result);
end;

procedure TVirtualMachine.OpDefineGlobal;
var
   Name   : TValueRecord;
   value  : TValueRecord;
begin
  name := FInstructionPointer.constant[NextInstruction];
  value := PeekStack;
  assert(GetIsString(name), 'name is not a string object');
  AddGlobal(GetString(name),value, false);
  popStack;
end;


(*function TVirtualMachine.InstructionPointer: TInstructionPointer;
begin
  result := FInstructionPointer;
end; *)

Constructor TVirtualMachine.Create(
  const results : TStrings);
var
  Value : TValueRecord;
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');
  FResults := results;

//  FInstructionPointer := TInstructionPointer.Create;

  //FCall := 0;

  FStack := TStack.Create;
  FStack.OnPush := CaptureStackPush;
  FStack.OnPop :=  CaptureStackPop;
 
  FHalt    := false;

  FGlobals.Init;

  RegisterNatives;

//  FFrames := TCallFrames.create;

end;



destructor TVirtualMachine.destroy;
begin
//   FInstructionPointer.Free;

   FStack.Free;

   FGlobals.finalize;

   //FFrames.free;

  
end;




end.
