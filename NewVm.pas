unit NewVm;

interface

uses
  sysutils,
  classes,
  LOXTypes,
  HashTable,
  values,
  opcodes,
  natives,
  AdapterCalls;
 
type

  TJumpTable = array[OP_NULL..OP_STORE_SUBSCR] of procedure of object;

  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);


  TFrame = record
    StackOffset   : integer;
    InstructionPointerIdx : integer;
    Fn         : pLoxFunction;
  end;


  TFrameItems = array of TFrame;


  (*TFrames = record
    Items : TFrameItems;
    StackTop : integer;
  end; *)

  TStackEvent = procedure(const value : TValueRecord) of object;


  TVirtualMachine = class
  private
    FJumpTable      : TJumpTable;

    FIndex          : TOpCodeValue; //current op code index

    FCodes          : TCodes; //current func op codes
    FCodeCount      : TOpCodeValue; //current func op code count
    FConstants      : TStack;  //current func constants

    FFrameStackTop  : integer;
    FFrameStackOffSet : integer;
   // FCurrentFrame : TFrame;

    FOnStackPush : TStackEvent;
    FOnStackPop  : TStackEvent;
    FRootFunction : TValueRecord;
    FHalt    : boolean;
    FStack   : TStack;
    FFrames : TFrameItems;

    //FGlobals : TGlobals;
    FResults : TStrings;
    procedure Execute;
//    function CurrentOpCode : integer;

    procedure Halt;
    procedure popFrame;
    function PopStack : TValueRecord;
    procedure PushStack(const value : TValueRecord);
    procedure RegisterNatives;
    procedure OpCall;
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
    procedure OPSetGlobal;
    procedure OpPOP;
    procedure OpGetLocal;
    procedure OPSetLocal;
    procedure HandleRunTimeError(const E: Exception);
    procedure OPJumpFalse;
    procedure OpJump;
//    procedure OpCall;
    procedure OpReturn;
    procedure OpBuildList;
    procedure OpIndexSubscriber;
    procedure OpStoreSubscriber;
    function Call(const Func : pLoxfunction; const ArgCount : Byte) : boolean;

    function increment(const index : integer) : boolean;
    function NextInstruction : TOpCodeValue;

  public
    function Run(const func : PLoxFunction) : TInterpretResult;
    constructor create(const results : TStrings);
    destructor Destroy; override;
    property OnPush : TStackEvent read FOnStackPush write FOnStackPush;
    property OnPop : TStackEvent read FOnStackPop write FOnStackPop;

  end;


implementation

uses
  dateutils,dialogs, addition, subtraction, valueManager;




function TVirtualMachine.increment(const index : integer) : boolean;
var
  newIdx : integer;
begin
  result := false;
  newIdx := FIndex + index;
  if (NewIdx >= 0) and (NewIdx < FCodeCount) then
  begin
    FIndex := NewIdx;
    result := true;
  end;
end;

function TVirtualMachine.NextInstruction: TOpCodeValue;
begin
  result := -1;

  if FCodeCount = 0 then exit;

  inc(FIndex);

  if FIndex = FCodeCount then exit;

  result :=  FCodes[FIndex];

end;


procedure TVirtualMachine.Execute;
var
  currentInstruction : integer;
begin
  currentInstruction := NextInstruction;

  while (CurrentInstruction <> -1) do
  begin

    if CurrentInstruction > (OP_STORE_SUBSCR) then exit;
    if @FJumpTable[CurrentInstruction] <> nil then
      FJumpTable[CurrentInstruction];
    CurrentInstruction := NextInstruction;


  end;
end;

function TVirtualMachine.Run(const func : PLoxFunction) : TInterpretResult;
begin
  FRootFunction := BorrowChecker.newValueFromFunction(rVM,Func);

  PushStack(FRootFunction);

  call(Func, 0);
                     
  Result := INTERPRET_NONE;

  if FCodeCount = 0 then exit;

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
  (* item   := PopStack;
   Index  := PopStack;
   ListValue := PopStack;
   //  Listvalue.List.Items[round(index.Number)] := Item;
   List := GetList(ListValue);
   List.Items[round(index.Number)] := Item;
   PushStack(Item); *)
end;

procedure TVirtualMachine.OpIndexSubscriber;
var
  indexValue, listValue, result: TValueRecord;
  index: Integer;
  List : pLoxList;
begin
 (*
  indexValue := PopStack;
  listValue := PopStack;
  index := round(indexValue.Number);
  List := GetList(ListValue);
  result := List.Items[index];
  PushStack(result); *)
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



(*function TVirtualMachine.CurrentOpCode : integer;
begin
  result :=  FInstructionPointer.Current;
end; *)

procedure TVirtualMachine.OpConstant;
begin
   PushStack(FConstants.items[NextInstruction]);


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
begin
   FStack.Less
end;


procedure TVirtualMachine.OPAdd;
begin
    FStack.Add;
end;


procedure TVirtualMachine.OPSubtract;
begin
  FStack.Subtract;
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
begin
  FResults.Add(GetString(PopStack));
end;




procedure TVirtualMachine.PopFrame;
begin
  assert(FFrameStackTop > 0, 'Frame stack top is zero, you can''t pop it');

  FFrameStackTop := FFrameStackTop - 1;   //pop frame

  if FFrameStackTop > 0 then
  begin

    //this is to decrease indirection. So we don't keep going through the pointer.
    if FFrames[FFrameStackTop-1].fn <> FFrames[FFrameStackTop].fn then
    begin
      FCodes        := FFrames[FFrameStackTop-1].fn.OpCodes.Codes;
      FCodeCount    := FFrames[FFrameStackTop-1].fn.OpCodes.Count;
      FConstants    := FFrames[FFrameStackTop-1].fn.Constants;

    end;
    FFrameStackOffSet := FFrames[FFrameStackTop-1].StackOffset;
    FIndex :=  FFrames[FFrameStackTop-1].InstructionpointerIdx;
  end;
end;


procedure TVirtualMachine.OpReturn;
var
  result : TValueRecord;
  offSet : integer;
begin

    Result := PopStack; //result of the function

    FStack.StackTop :=  FFrames[FFrameStackTop-1].StackOffset;

    PushStack(result); //push result to new location in stack.

    PopFrame;
 end;


procedure TVirtualMachine.OpCall;
var
  ArgCount : byte;
  Value : TValueRecord;
begin
   ArgCount := NextInstruction;

        Value := FStack.peek(ArgCount);

        if Value.Kind = lxFunction then
        begin
          Assert(call(pLoxFunction(Value.Obj), argCount) = true, 'failed to execute function call');

        end;
end;

function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : byte) : boolean;

begin
  result := false;


  if func = nil then exit;

  if (argCount <> func.Arity) then exit;

  if FFrameStackTop > high(Byte) then exit;

  if FStack.StackTop-ArgCount-1 < 0 then exit;

  FFrameStackOffSet := FStack.StackTop-ArgCount-1;


  if FFrameStackTop > 0 then
  begin
    //bookmark where the soon to be previous instruction pointer is to return to later.
    FFrames[FFrameStackTop-1].InstructionPointerIdx := FIndex;
  end;

  //this is just to cut down on indirection

  if Func <> FFrames[FFrameStackTop-1].fn then    //there's no point resetting these if it's the same(i.e. in recursive calls)
  begin
    FCodes        := Func.OpCodes.Codes;
    FCodeCount    := Func.OPCodes.Count;
    FConstants    := Func.Constants;
  end;


  //set up new call frame
  with FFrames[FFrameStackTop] do
  begin
    Fn := Func;
    StackOffset := FFrameStackOffSet;
  end;

  FFrameStackTop := FFrameStackTop + 1;

  FIndex := -1; //reset to point to the current funcs opcode starting at zero (after move next);

  result := true;
end;


procedure TVirtualMachine.OPLoop;
begin
   assert(Increment((-NextInstruction)) = true, 'failed to move to loop offset');
end;

procedure TVirtualMachine.OPJump;
begin
   assert(Increment(NextInstruction) = true, 'failed to move to jump offset');
end;


procedure TVirtualMachine.OpJumpFalse;
begin
   if (FStack.IsFalse) then   //check top of stack for truthiness
   begin
      OpJump;
   end
   else
   begin
      NextInstruction;//skip the operand.
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
    (*
    R := PopStack;
    Assert(GetNumber(R) <> 0); //divide by zero exceptions.
    L := PopStack;

    Result := BorrowChecker.NewNumber(GetNumber(L) / GetNumber(R));

    PushStack(Result);
    *)
end;

procedure TVirtualMachine.OPNotEqual;
begin
  
  //PushStack(BorrowChecker.NewBool(isFalsey(PopStack)));
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



procedure TVirtualMachine.OPSetLocal;
begin
 // FStack.setItem(FFrameStackOffSet + NextInstruction,FStack.Peek);  //noting that next instruction here is the index of the local on the stack? Still a bit hazy on locals
  FStack.Copy(FStack.StackTop-1,FFrameStackOffSet + NextInstruction);
end;

procedure TVirtualMachine.OpGetLocal;
begin
  //PushStack(FStack[FFrameStackOffSet + NextInstruction]);

  FStack.Copy(FFrameStackOffSet + NextInstruction,FStack.StackTop);
  FStack.StackTop := FStack.StackTop + 1;
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


procedure TVirtualMachine.OpDefineGlobal;
var
  NameValue : TNameValue;
begin
  nameValue.Name := GetString(Fconstants.Items[NextInstruction]);
  NameValue.Value := FStack.peek;
  FGlobals.add(NameValue);
  popStack;
end;

procedure TVirtualMachine.OPGetGlobal;
var
  NameValue : TNameValue;

begin
  if FGlobals.Find(GetString(FConstants.Items[NextInstruction]),NameValue) then
  begin
    PushStack(NameValue.Value);
    exit;
  end;

  Showmessage('Could not locate global name');
end;


procedure TVirtualMachine.OPSetGlobal;
var
   NameValue : TNameValue;
   Index : integer;

begin

  nameValue.Name := GetString(FConstants.Items[NextInstruction]);
  NameValue.Value := FStack.peek;

  Index := FGlobals.IndexOf(NameValue);

  if Index >= 0 then
  begin
    FGlobals.setValue(Index,NameValue);
  end;

end;

procedure TVirtualMachine.OpPOP;
begin
  FStack.pop;
end;



function TVirtualMachine.PopStack : TValueRecord;
begin
  Result := FStack.Pop;
  //if assigned(FOnStackPop) then FOnStackPop(result);
end;

procedure TVirtualMachine.PushStack(const value: TValueRecord);
begin
  FStack.Push(Value);
  //if assigned(FOnStackPush) then FOnStackPush(value);
end;


constructor TVirtualMachine.Create(
  const results : TStrings);
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');
  SetLength(FFrames,256);

  FResults := results;
  FStack.Init;
  FHalt    := false;
  FGlobals.init;
  RegisterNatives;

    FJumpTable[OP_CALL] := opCall;
    FJumpTable[OP_CONSTANT ] :=  OpConstant;
    FJumpTable[OP_CALL ] :=   OpCall;
    FJumpTable[OP_GET_GLOBAL] :=  OPGetGlobal;
    FJumpTable[OP_GET_LOCAL] :=  OpGetLocal;
    FJumpTable[OP_SET_LOCAL] :=  OPSetLocal;
    FJumpTable[OP_Return ] :=  OpReturn;
    FJumpTable[OP_ADD ] :=  OpAdd;
    FJumpTable[OP_SUBTRACT ] :=  OpSubtract;
    FJumpTable[OP_POP ] :=  OpPOP;
    FJumpTable[OP_DEFINE_GLOBAL] :=  OpDefineGlobal;
    FJumpTable[OP_SET_GLOBAL] :=  OPSetGlobal;
    FJumpTable[OP_BUILD_LIST ] :=  OpBuildList;
    FJumpTable[OP_INDEX_SUBSCR ] :=  OpIndexSubscriber;
    FJumpTable[OP_STORE_SUBSCR ] :=  OpStoreSubscriber;
    FJumpTable[OP_Nil ] :=  OpNil;
    FJumpTable[OP_TRUE ] :=  OpTrue;
    FJumpTable[OP_FALSE ] :=  OpFalse;
    FJumpTable[OP_GREATER ] :=  OPgreater;
    FJumpTable[OP_LESS ] :=  OPless;
    FJumpTable[OP_EQUAL ] :=  OPEqual;
    FJumpTable[OP_NOT ] :=  OPNotEqual;
    FJumpTable[OP_DIVIDE ] :=  OPdivide;
    FJumpTable[OP_MULTIPLY ] :=  OPMultiply;
    FJumpTable[OP_NEGATE ] :=  OpNegate;
    FJumpTable[OP_PRINT  ] :=  OPPrint;
    FJumpTable[OP_JUMP_IF_FALSE ] :=  OPJumpFalse;
    FJumpTable[OP_JUMP] :=  OpJump;
    FJumpTable[OP_LOOP] :=  Oploop;




end;



destructor TVirtualMachine.destroy;
begin
//  FStack.Free;
end;




end.
