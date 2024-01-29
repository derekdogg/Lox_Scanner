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

  
  TCallRecord = record
    Fn          : pLoxFunction;
    Name        : string; //function Name + params it was called with.
  end;

  TFrame = record
    StackOffset   : integer;
    InstructionPointerIdx : integer;
    CallRecord : TCallRecord;
  end;



  TFrameItems = array of TFrame;


  (*TFrames = record
    Items : TFrameItems;
    StackTop : integer;
  end; *)

  TStackEvent = procedure(const value : TValueRecord) of object;


  TVirtualMachine = record
  private
    FOnStackPush : TStackEvent;
    FOnStackPop  : TStackEvent;
    FHalt    : boolean;
    FResults : TStrings;

    //this prob should be renamed. These are here so I can inline next instruction
    LCurrentCode    : integer;
    LIndex          : TOpCodeValue; //current op code index
    LCodes          : TCodes; //current func op codes
    LCodeCount      : TOpCodeValue; //current func op code count
    // end of vars used for inline
    function subtract(const a,b : TValueRecord; var return : TValueRecord) : boolean;
    procedure Halt;
    procedure RegisterNatives;

    procedure HandleRunTimeError(const E: Exception);
    procedure OpBuildList;
    procedure OpIndexSubscriber;
    procedure OpStoreSubscriber;

    function increment(const index : integer) : boolean;inline;

    function NextInstruction: TOpCodeValue; inline;
    function add(const a,b : TValueRecord; var return : TValueRecord) : boolean;
  public
//
    procedure Execute(const func : PLoxFunction);

    constructor create(const results : TStrings);
    //destructor Destroy; override;
    property OnPush : TStackEvent read FOnStackPush write FOnStackPush;
    property OnPop : TStackEvent read FOnStackPop write FOnStackPop;

  end;


implementation

uses
  dateutils,dialogs, valueManager;

 function SubtractString(original, toSubtract: string): string;
var
  i, j: Integer;
  resultString: string;
begin
  resultString := original;

  for i := 1 to Length(toSubtract) do
  begin
    j := Pos(toSubtract[i], resultString);
    if j > 0 then
      System.Delete(resultString, j, 1);
  end;

  Result := resultString;
end;


function TVirtualMachine.subtract(const a,b : TValueRecord; var return : TValueRecord) : boolean;
begin
    result := false;

    case a.Kind of
      lxShort,lxString: //if the left hand side is a string the result will always be a string
      begin
        return := bc.NewString(SubtractString(getString(a), getString(b)));
        result := true;
      end;


      lxNumber:
      begin
        case b.Kind of
          lxNumber:
          begin
            //both numbers here
            return := bc.NewNumber(a.Number - b.Number);
            result := true;
          end;
        end;
      end;


    end;
  end;

function TVirtualMachine.add(const a,b : TValueRecord; var return : TValueRecord) : boolean;
  begin
    result := false;

    case a.Kind of
      lxShort,lxString: //if the left hand side is a string the result will always be a string
      begin
        return := bc.NewString(getString(a) + getString(b));
        result := true;
      end;

      lxBoolean :
      begin
        case b.Kind of
          lxBoolean : //both boolean addition
          begin
            return := bc.NewBool(a.bool and b.Bool);
            result := true;
          end;
        end;

      end;

      lxNumber:
      begin
        case b.Kind of
          lxNumber:
          begin
            //both numbers here
            return := bc.NewNumber(a.Number + b.Number);
            result := true;
          end;


          lxShort,lxBoolean,lxString, lxNull:
          begin
            return := bc.NewString(getString(a) + getString(b));
            result := true;
          end;
        end;
      end;

      lxNull:
      begin
        return := b;  //is this what you want here?
        result := true; // a = nil; b = true; result = b;
      end;
    end;
  end;

function TVirtualMachine.increment(const index : integer) : boolean;
var
  newIdx : integer;
begin
  result := false;
  newIdx := LIndex + index;
  if (NewIdx >= 0) and (NewIdx < LCodeCount) then
  begin
    LIndex := NewIdx;
    result := true;
  end;
end;

function TVirtualMachine.NextInstruction: TOpCodeValue;
begin
  result := -1;
  if LCodeCount = 0 then exit;
  inc(LIndex);
  if LIndex = LCodeCount then exit;
  LCurrentCode := LCodes[LIndex];
  result :=  LCurrentCode;
end;

procedure TVirtualMachine.Execute(const func : PLoxFunction);
var
  LCurrentFn    : pLoxfunction;
  LCurrentFnVal : TValueRecord;
  LRootFunction : TValueRecord;

  LCallValue : TValueRecord;
  LCallFunc  : pLoxFunction;
  LArgCount  : byte;
  LNameValue : TNameValue;

  LStack : TStack;

  LGlobalIndex : integer;
  LGlobals  : TGlobals;
  LReturnValue : TValueRecord;

  LConstants      : TStack;  //current func constants

  LFrames : TFrameItems;
  LFrameStackTop  : integer;
  LFrameStackOffSet : integer;

  LCurrentInstruction : integer;

  LCallCount : integer;

function UniqueFuncName(const func : pLoxFunction) : String; //name of func, followed by name of all params
var
  i : integer;
  paramStr : String;
begin
  //we assume here that this routine is called when in a suitable stack state.
  paramStr := '';
  result :=  func.Name;
  if func.Arity > 0 then
  begin
    for i := func.Arity-1 downto 0 do
    begin
      paramStr := ParamStr + GetString(LStack.Peek(i));
    end;
    result := result + paramstr;
  end;
end;

procedure OpAdd;
var a,b : TValueRecord;
begin
 a := LStack.Pop;
 b := LStack.Pop;
 assert(Add(a,b,b), 'failed to add on the stack');
 LStack.Push(b);

end;

function isFalsey(const Value : TValueRecord) : boolean;
  begin
    result := false;

    case Value.Kind of
      lxBoolean : result := Value.Bool = false;
      lxNumber  : result := Value.Number <= 0;
      lxString  : result := lowercase(trim(GetString(Value))) = 'false';
      lxNull    : result := true;
    end;
  end;

procedure OPNotEqual;
begin
  LStack.Push(bc.NewBool(isFalsey(LStack.Pop)));
end;




procedure OpSubtract;
var a,b : TValueRecord;
begin
 b := LStack.Pop;
 a := LStack.Pop;
 assert(subtract(a,b,a), 'failed to add on the stack');
 LStack.Push(a);
end;

procedure OPGreater;
var
  L,R : TValueRecord;
begin
    R := LStack.Pop;
    L := LStack.Pop;
    LStack.Push(bc.NewBool(l.Number > r.Number));
end;

procedure OpEqual;
begin
  LStack.Push(bc.NewBool(GetString(LStack.pop) = GetString(LStack.Pop)));
end;

procedure OPLess;
var
  L,R : TValueRecord;
begin
  R := LStack.Pop;
  L := LStack.Pop;
  LStack.Push(bc.NewBool(l.Number < r.Number));
end;

procedure OpMultiply;
var
  L,R : TValueRecord;
  Result : TValueRecord;

begin
    R := LStack.Pop;
    L := LStack.Pop;

    if getIsNumber(L) and getIsNumber(R) then
    begin
      result := bc.NewNumber(GetNumber(L) * GetNumber(R));
      LStack.Push(Result);
    end;

end;

procedure OpNegate;
var
  Result : TValueRecord;
  R : TValueRecord;
begin

    R := LStack.Pop;

    if (GetIsNumber(R)) then
    begin
      Result := bc.newNumber(- R.Number);
      LStack.Push(Result);
    end;
end;

procedure OpDivide;
var
   L,R, Result : TValueRecord;
begin

    R := LStack.Pop;
    Assert(GetNumber(R) <> 0, 'division by zero error'); //divide by zero exceptions.
    L := LStack.Pop;

    Result := bc.NewNumber(GetNumber(L) / GetNumber(R));

    LStack.Push(Result);

end;



procedure opReturn;
begin
          //PUSH Result
          LReturnValue := LStack.Pop; //result of the function

          LNameValue.Name := LFrames[LFrameStackTop-1].CallRecord.Name;


          //this part here is about updating the name of the function called earlier, with the actual value we have finally calculated.
          //this in turn is then used to short-circuit to the result immediately if the result is already known. (i.e same func called multiple times with the same params).
          LGlobalIndex := LGlobals.IndexOf(LNameValue);
          if LGlobalIndex > 0 then
          begin
             LNameValue.Value := LReturnValue;
             LGlobals.SetValue(LGlobalIndex,LNameValue);
          end;

          LStack.StackTop :=  LFrames[LFrameStackTop-1].StackOffset;

          LStack.Push(LReturnValue); //push result to new location in stack.

          //POP FRAME
          assert(LFrameStackTop > 0, 'Frame stack top is zero, you can''t pop it');

          dec(LFrameStackTop);   //pop frame

          if LFrameStackTop > 0 then
          begin

            //this is to decrease indirection. So we don't keep going through the pointer.
            if LFrames[LFrameStackTop-1].CallRecord.fn <> LCurrentFn then//LFrames[LFrameStackTop].fn then
            begin
              LCurrentFn    := LFrames[LFrameStackTop-1].CallRecord.fn;
              LCurrentFnVal := bc.NewValueFromFunction(LCurrentFn);
              LCodes        := LCurrentFn.OpCodes.Codes;
              LCodeCount    := LCurrentFn.OpCodes.Count;
              LConstants    := LCurrentFn.Constants;
            end;


            LFramestackOffSet := LFrames[LFrameStackTop-1].StackOffset;
            LIndex :=  LFrames[LFrameStackTop-1].InstructionpointerIdx;
          end;
end;

function Call(
  const Func : pLoxfunction;
  const ArgCount : byte) : boolean;

begin
  result := false;
  inc(LCallCount);
  if func = nil then exit;

  if func <> LCurrentFn then
  begin
    LCurrentFn := func;
    LCurrentFnVal := bc.NewValueFromFunction(LCurrentFn);
    LCodes        := Func.OpCodes.Codes;
    LCodeCount    := Func.OPCodes.Count;
    LConstants    := Func.Constants;
  end;

  if (argCount <> func.Arity) then exit;

  if LFrameStackTop > high(Byte) then exit;


  LFrameStackOffSet := LStack.StackTop-ArgCount-1;
  if LFrameStackOffSet < 0 then exit;

  if LFrameStackTop > 0 then
  begin
    //bookmark where the soon to be previous instruction pointer is to return to later.
    LFrames[LFrameStackTop-1].InstructionPointerIdx := LIndex;
  end;

  with LFrames[LFrameStackTop] do
  begin
    CallRecord.Fn := Func; //the only reason we use the fn, is to track what to set locally for codes, on a return, begging the question do we need it?
    CallRecord.Name := UniqueFuncName(LCurrentFn);
    StackOffset := LFramestackOffSet;
  end;

  LFrameStackTop := LFrameStackTop + 1;

  LIndex := -1; //reset to point to the current funcs opcode starting at zero (after move next);

  result := true;
end;

begin
  LCallCount := 0;
  SetLength(LFrames,256);
  LStack.Init;
  LIndex := 0;

  LFrameStackTop  := 0;
  LFrameStackOffSet := 0;


  LGlobals.Init;
  LGlobalIndex := 0;

  LRootFunction := bc.newValueFromFunction(Func);

  LStack.Push(LRootFunction);

  call(Func, 0);

  if LCodeCount = 0 then exit;

  LCurrentInstruction := NextInstruction;
 
  while (LCurrentInstruction <> -1) do
  begin
    case LCurrentInstruction of  //memoization technique?

    OP_CALL : begin
        LArgCount := NextInstruction; //here we get the number of arguments
        LCallValue := LStack.peek(LArgCount); //then we skip back a few places to where the args start, to get the function call
        Assert(LCallValue.Kind = lxFunction,'trying to call a function but the value on the stack isn''t');


        (* what happens here is that I record a unique name for the function based on the name and it's params passed in

            for example,
                fib(40) = fib40
                fib(38) = fib38
                etc,

                foo(1,2,3,4) = foo1234

            then I record the name of the call in the hash table (without the final result (currently unknown)

            later, when the function returns, I update the function in the hashtable with the result

            this is done so that when a function we know the result of already is called again, like in a recursive fib sequence,
            we can jump to the result immediately, rather than pushing millions of times onto a stack frame

            there is a trade-off between speed (i.e. fib 55 in 0ms) and a full stack trace.

            But since we haven't even got the facility to do stack tracing yet, I figure it's worth it.

            What I'm thinking is that this "feature" could be turned on or off with compiler directive...

            i dunno yet, just experimenting at the moment.

        *)
        LNameValue.Name := UniqueFuncName(LCallFunc);
        LGlobalIndex := LGlobals.Indexof(LNameValue);
        if LGlobalIndex = -1 then
        begin
          LGlobals.Add(LNameValue);
          LCallFunc  := pLoxFunction(LCallValue.Obj);
          Assert(call(LCallFunc, LArgCount) = true, 'failed to execute function call');

        end
        else
        begin //we have already presumably calculated this value (as per notes above), now we just fetch it directly from the hashtable)
          LReturnValue := LGlobals.Get(LGlobalIndex).Value;

          LStack.StackTop := LStack.StackTop-LArgCount-1; // instead of creating a new frame, we basically discard the params and push the result back on the stack at new location

          LStack.Push(LReturnValue); //push result to new location in stack.

        end;
     end;


    OP_CONSTANT  :  LStack.Push(LConstants.items[NextInstruction]);

    OP_GET_LOCAL :  //push frame local at top of stack(e.g beyond current stacktop) to stack
    begin
      LStack.Copy(LFramestackOffSet + NextInstruction,LStack.StackTop);
      inc(LStack.StackTop);// := LStack.StackTop + 1;
    end;

    //this is basically the equivalent of frame->slots[slot] = peek(0);
    OP_SET_LOCAL : LStack.Copy(LStack.StackTop-1,LFramestackOffSet + NextInstruction);

    OP_Return  : opreturn;


    OP_ADD  : OpAdd;

    OP_SUBTRACT  :  opSubtract;

    OP_POP  : LStack.pop;

    OP_GET_GLOBAL :
    begin
      LCurrentInstruction := NextInstruction;
      //basically, here we check for recursive calls. if the we can use the LCurrentFn it will probably be faster than looking for in Hash table.
      //I need though to check veracity of this approach.
      if GetString(LConstants.Items[LCurrentInstruction]) = LCurrentFn.Name then
      begin
         LStack.Push(LCurrentFnVal); //since we also record its equiv value we push it straight on stack
      end
      else
      if LGlobals.Find(GetString(LConstants.Items[LCurrentInstruction]),LNameValue) then
      begin
        LStack.Push(LNameValue.Value);
      end
      else
      begin
        Showmessage('Could not locate global name');
      end;
    end;

    OP_DEFINE_GLOBAL :
    begin
        LNameValue.Name := GetString(LConstants.Items[NextInstruction]);
        LNameValue.Value := LStack.pop;  //peek
        LGlobals.add(LNameValue);
       // LStack.Pop;
    end;

    OP_SET_GLOBAL :
    begin
      LnameValue.Name := GetString(LConstants.Items[NextInstruction]);
      LNameValue.Value := LStack.peek;

      LGlobalIndex := LGlobals.IndexOf(LNameValue);
      if LGlobalIndex >= 0 then
      begin
        LGlobals.setValue(LGlobalIndex,LNameValue);
      end;

    end;
    OP_BUILD_LIST  :  OpBuildList;
    OP_INDEX_SUBSCR  :  OpIndexSubscriber;
    OP_STORE_SUBSCR  :  OpStoreSubscriber;
    OP_Nil  :   LStack.Push(bc.NewNil);
    OP_TRUE  :  LStack.Push(bc.newBool(true));
    OP_FALSE  : LStack.Push(bc.newBool(false));
    OP_GREATER  :  OPgreater;
    OP_LESS  :  OpLess;
    OP_EQUAL: OpEqual;
    OP_NOT  :  OPNotEqual;
    OP_DIVIDE  :  OPdivide;
    OP_MULTIPLY  :  OPMultiply;
    OP_NEGATE  :  OpNegate;
    OP_PRINT   :  FResults.Add(GetString(LStack.Pop));


    OP_JUMP : assert(Increment(NextInstruction) = true, 'failed to move to jump offset');

    OP_LOOP : assert(Increment(-NextInstruction) = true, 'failed to move to loop offset');

    OP_JUMP_IF_FALSE  :
    begin
      if (IsFalsey(LStack.Peek)) then   //check top of stack for truthiness
      begin
        assert(Increment(NextInstruction) = true, 'failed to move to jump offset');

      end
      else
      begin
        NextInstruction;//skip the operand.
      end;

    end;

    end;
    LCurrentInstruction := NextInstruction;
  end;
  //LStack.Pop;

  //Showmessage(Inttostr(LCallCount));
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
  (* item   := LStack.Pop;
   Index  := LStack.Pop;
   ListValue := LStack.Pop;
   //  Listvalue.List.Items[round(index.Number)] := Item;
   List := GetList(ListValue);
   List.Items[round(index.Number)] := Item;
   LStack.Push(Item); *)
end;

procedure TVirtualMachine.OpIndexSubscriber;
var
  indexValue, listValue, result: TValueRecord;
  index: Integer;
  List : pLoxList;
begin
 (*
  indexValue := LStack.Pop;
  listValue := LStack.Pop;
  index := round(indexValue.Number);
  List := GetList(ListValue);
  result := List.Items[index];
  LStack.Push(result); *)
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
   NewList :=  bc.newValueList(true,'');

   // Add items to list
  // LStack.Push(value); // So list isn't swept by GC in appendToList - [to do!!]

   List := GetList(NewList);
   for  i:= itemCount downto 1 do
   begin
      Item := PeekStack(i-1);
      List.Items.Add(Item);
   end;

  // LStack.Pop;

   // Pop items from stack
   while itemCount > 0 do
   begin
      LStack.Pop;
      Dec(itemCount);
    end;

   LStack.Push(NewList);   *)
end;



//suppose we expect 3 values;
function Foo(const ArgCount: Integer;const Values : TStack): TValueRecord;
var
  v1,v2,v3 : TValueRecord;
begin
  v1 := values.Peek(2);
  v2 := values.Peek(1);
  v3 := Values.Peek(0);

  result := bc.newString(GetString(v1) + GetString(v2) + GetString(v3));
end;

procedure TVirtualMachine.Halt;
begin
  FHalt := true;
end;

procedure TVirtualMachine.RegisterNatives;
begin

(*   AddGlobal( ('foobar'),bc.NewNative(foo), true);
   AddGlobal( ('DateTime'),bc.NewNative(DateTime), true);
   AddGlobal( ('FileExists'),bc.NewNative(FileExists), true);
   AddGlobal( ('LoadFromFile'),bc.NewNative(LoadStringFromFile), true);
  *)
end;



constructor TVirtualMachine.Create(
  const results : TStrings);
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');

  FResults := results;
  FHalt    := false;

  RegisterNatives;

end;







end.
