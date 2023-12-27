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

    FCodeCount      : integer;
    FIndex          : integer;
    FCodes          : TOpCode;
    FConstants      : TStack;
    FFrameStackTop  : integer;
    FFrameStackOffSet : integer;
   // FCurrentFrame : TFrame;

    FOnStackPush : TStackEvent;
    FOnStackPop  : TStackEvent;
    FRootFunction : TValueRecord;
    FHalt    : boolean;
    FStack   : TStack;
    FFrames : TFrameItems;

    FGlobals : TGlobals;
    FResults : TStrings;
    procedure Execute;
//    function CurrentOpCode : integer;

    procedure Halt;
    procedure  popFrame;
    function PopStack : TValueRecord;
    procedure PushStack(const value : TValueRecord);
    procedure RegisterNatives;
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

    function Getconstant(const Index : integer) : TValueRecord;
    function GetValue(const Index : integer) : Integer;
    procedure setIndex(const Value: integer);
    function Move(const index : integer) : boolean;
    function increment(const index : integer) : boolean;
    function NextInstruction : Integer;

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


function  TVirtualMachine.GetValue(const Index : integer) : integer;
begin
  assert((index >= 0) and (index < FCodeCount));
  result := FCodes[FIndex];
end;

function TVirtualMachine.Getconstant(const Index: integer): TValueRecord;
begin
  result := FConstants[Index];
end;


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

function TVirtualMachine.Move(const index: integer): boolean;
begin
  result := false;
  if (index >= 0) and (index < FCodeCount) then
  begin
    FIndex := index;
    result := true;
  end;
end;


function TVirtualMachine.NextInstruction: integer;
begin
  result := -1;

  if FCodeCount = 0 then exit;

  inc(FIndex);

  if FIndex = FCodeCount then exit;

  result :=  FCodes[FIndex];
end;

 

procedure TVirtualMachine.setIndex(const Value: integer);
begin
  Assert(FCodeCount > 0, 'can''t set index as there is no opcodes');
  Assert(Value >= -1, 'opcode index is less than -1');
  Assert(Value < FCodeCount, 'op code index is > FCodeCount');
  FIndex := Value;
end;



procedure TVirtualMachine.Execute;
var
  CurrentInstruction : integer;
  ArgCount : byte;
  Value : TValueRecord;

begin

  CurrentInstruction := NextInstruction;

  while
        (FFrameStackTop > 0) and
        (CurrentInstruction <> -1) do
  begin

   //if CurrentInstruction = OP_CALL then OpCall
   //else

    Case CurrentInstruction of


    OP_CONSTANT : OpConstant;

    OP_CALL : begin


        ArgCount := NextInstruction;

        Value := FStack.peek(ArgCount);

        if Value.Kind = lxFunction then
        begin
          call(pLoxFunction(Value.Obj), argCount);

        end;

    end;




    OP_GET_GLOBAL: OPGetGlobal;

    OP_GET_LOCAL: OpGetLocal;

    OP_SET_LOCAL: OPSetLocal;

    OP_Return : OpReturn;  

    OP_ADD : OpAdd;

    OP_SUBTRACT : OpSubtract;

    OP_POP : OpPOP;

    OP_DEFINE_GLOBAL: OpDefineGlobal;

    OP_SET_GLOBAL: OPSetGlobal;

    OP_BUILD_LIST : OpBuildList;

    OP_INDEX_SUBSCR : OpIndexSubscriber;

    OP_STORE_SUBSCR : OpStoreSubscriber;

    OP_Nil : OpNil;

    OP_TRUE : OpTrue;

    OP_FALSE : OpFalse;

    OP_GREATER : OPgreater;

    OP_LESS : OPless;

    OP_EQUAL : OPEqual;

    OP_NOT : OPNotEqual;
 
    OP_DIVIDE : OPdivide;

    OP_MULTIPLY : OPMultiply;

    OP_NEGATE : OpNegate;

    OP_PRINT  : OPPrint;

    OP_JUMP_IF_FALSE : OPJumpFalse;

    OP_JUMP: OpJump;

    OP_LOOP: Oploop;


    end;
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



(*function TVirtualMachine.CurrentOpCode : integer;
begin
  result :=  FInstructionPointer.Current;
end; *)

procedure TVirtualMachine.OpConstant;
begin
   PushStack(FConstants[NextInstruction]);
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
  assert(FFrameStackTop > 0);

  FFrameStackTop := FFrameStackTop - 1;   //pop frame

  if FFrameStackTop > 0 then
  begin
    //FCurrentFrame := FFrames[FFrameStackTop-1];

    //FFunction     := FFrames[FFrameStackTop-1].fn;
    if FFrames[FFrameStackTop-1].fn <> FFrames[FFrameStackTop].fn then
    begin
      FCodes        := FFrames[FFrameStackTop-1].fn.Codes;
      FCodeCount    := FFrames[FFrameStackTop-1].fn.Codes.Count;
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

    (*Result := PopStack; //result of the function

    FStack.StackTop :=  FFrames[FFrameStackTop-1].StackOffset;

    PushStack(result); //push result to new location in stack.

    PopFrame; *)



    FStack.Copy(FStack.StackTop-1,FFrameStackOffSet);
    FStack.StackTop := FFrameStackOffSet + 1;
    PopFrame;

end;


function TVirtualMachine.Call(
  const Func : pLoxfunction;
  const ArgCount : byte) : boolean;

begin
  result := false;

 (* if func = nil then exit;

  if (argCount <> func.Arity) then exit;

  if FFrameStackTop > high(Byte) then exit;

  if FStack.StackTop-ArgCount-1 < 0 then exit;

  *)


  if FFrameStackTop > 0 then
  begin
    //bookmark where the instruction pointer is for the current callframe, to return to later.
    FFrames[FFrameStackTop-1].InstructionPointerIdx := FIndex;
  end;


  //set up new call frame now
  with FFrames[FFrameStackTop] do
  begin
    Fn := Func;
    StackOffset := FStack.StackTop-ArgCount-1;
  end;

  //this is just to cut down on indirection
  FCodes        := Func.Codes;
  FCodeCount    := Func.Codes.Count;
  FConstants    := Func.Constants;
  FFrameStackOffSet := FFrames[FFrameStackTop].StackOffset;
  // end of indirection

  FFrameStackTop := FFrameStackTop + 1;

  FIndex := -1; //reset to point to the current funcs opcode starting at zero (after move next);

  result := true;
end;


procedure TVirtualMachine.OPLoop;
begin
   assert(Increment(-NextInstruction) = true, 'failed to move to loop offset');
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
  nameValue.Name := GetString(Fconstants[NextInstruction]);
  NameValue.Value := FStack.peek;
  FGlobals.add(NameValue);
  popStack;
end;

procedure TVirtualMachine.OPGetGlobal;
var
   NameValue : TNameValue;
   Index : integer;
   ms : integer;

begin

  //look up value in hash table to see if it exists
  NameValue.Name := GetString(FConstants[NextInstruction]);


  index := FGlobals.IndexOf(NameValue);
  assert(Index >= 0, 'Failed to locate global : ' + NameValue.Name);
  PushStack(FGlobals.Get(Index).Value);



end;


procedure TVirtualMachine.OPSetGlobal;
var
   NameValue : TNameValue;
   Index : integer;

begin

  nameValue.Name := GetString(FConstants[NextInstruction]);
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
end;



destructor TVirtualMachine.destroy;
begin
//  FStack.Free;
end;




end.
