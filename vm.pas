unit vm;

interface

uses
  sysutils,
  classes,
  Chunk,
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
    FNatives : TNatives;
    FFrames : TCallFrames;
    FFrame : TCallFrame;

    FStackResults : TValueList;  //keep track of new values added to stack.For disposal later.
    //FStack  : TValueStack; //byte code stack
    FGlobals : TValuePairs;




    FResults : TStrings;

    procedure ClearLog;
    procedure Log(Const txt : string);
    function PopStack : pValue;
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
    procedure DoGetLocal;
    procedure DoSetLocal;
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
       //const Natives     : TNatives;
       const Constants   : TValueList;
       const Globals     : TValueList;
       const LoxFunction : pLoxFunction;
       const results : TStrings);
     procedure finalize;



  end;

implementation

uses
  dialogs, addition;


{ TVirtualMachine }
(*function TVirtualMachine.Result: TByteCode;
begin
  assert(FFrames.Stack.Count = 1, 'stack is empty');
  result := FFrames.Stack.Pop;
end;*)

function TVirtualMachine.Run : TInterpretResult;
begin
  
  //CallFrame* frame = &vm.frames[vm.frameCount - 1];

  clearLog;

  Log('Run');
  Result := INTERPRET_NONE;
  if FFrames.Frame.InstructionPointer.Count = 0 then exit;
  while FFrames.Frame.InstructionPointer.Next <> nil do
  begin

    //ByteCode.Operation := TOpCodes(FFrames.Frame.InstructionPointer.CurrentByte^);

    Case TOpCodes(FFrames.Frame.InstructionPointer.CurrentByte^) of

      OP_CONSTANT : begin
         DoConstant;
      end;


      OP_DEFINE_GLOBAL: begin
          DoGlobal;
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


      OP_GET_LOCAL:
      begin
        doGetLocal;
      end;

      OP_SET_LOCAL:
      begin
        DoSetLocal;
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
        DoAdd;
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


      OP_JUMP_IF_FALSE:
      begin
        JumpFalse;
      end;

      
      OP_JUMP:
      begin
        Jump;
      end;


       OP_LOOP:
       begin
         loop;
       end;

      OP_CALL :
      begin
         DoCall; //note here this will create a new stack frame, create a new IP. Something to be aware of.
      end;


      OP_Return :
      begin
        DoReturn;
      end;

    end;

  end;
  Result := INTERPRET_OK;
end;


procedure TVirtualMachine.HandleRunTimeError(const E : Exception);
begin
  showmessage(E.message);
end;

procedure TVirtualMachine.DoConstant;
var
  constantIndex : integer;
  Value : pValue;
begin
   Log('DoConstant');
   MoveNext;
   constantIndex := FFrames.Frame.InstructionPointer.CurrentByte^;
   value := FFrames.Frame.InstructionPointer.Constant[constantIndex];
   FFrames.Stack.Push(value);
end;


procedure TVirtualMachine.Greater;
var
  L,R,Result : pValue;
begin
  Log('greater');
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_GREATER));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;

    Result := NewBool(l.Number > r.Number);
    FFrames.Stack.Push(result);
    FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Less;
var
  L,R, Result : pValue;
begin
  Log('less');
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_LESS));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;
    Result := NewBool(l.Number < r.Number);
    FFrames.Stack.Push(Result);
    FStackResults.Add(Result);
 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.DoAdd;
var
  L,R : pValue;
  Result : pValue;
begin
  result := nil;
  Log('Add');
  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;

    result := TAddition.Add(L,R);
    if assigned(result) then FFrames.Stack.Push(result);

  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


function SubtractStrings(original, toSubtract: string): string;
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

procedure TVirtualMachine.subtract;
var
  L,R : pValue;
  
begin
  Log('Subtract');

  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_SUBTRACT));
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;

    if (L.IsNumber) and (R.IsNumber) then
    begin
      L.number := L.Number - R.Number;
      FFrames.Stack.Push(L);
      exit;
    end;

    if (L.IsString) and (R.IsString) then
    begin
      L.Str := SubtractStrings(L.toString,R.ToString);//StringReplace(l.tostring,r.tostring, '', [rfReplaceAll]);    //this is a bit of a curiosity, it doesn't just deduct 1 time, it does it multiple times...
      FFrames.Stack.Push(L);
      exit;
    end;

    if (L.IsString) and (R.IsNumber) then
    begin
      L.Str := Copy(L.Str,0, Length(L.Str) - round(R.Number));
      FFrames.Stack.Push(L);
      exit;
    end;

    if (L.IsNumber) and (R.IsString) then
    begin
      //What goes here?
      raise exception.create('trying to subtract a string from a number'); //not sure yet what to put here.
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
  Log('Multiply');
  //we assume here we're sitting on an OP_MULTIPLY in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_MULTIPLY));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;

    if (L.IsNumber) and (R.IsNumber) then
    begin

      L.Number := L.Number * R.Number;
      FFrames.Stack.Push(L);
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
       FFrames.Stack.Push(L);
     end;

     if (R.IsString) and (L.IsNumber) then
     begin
       s := '';
       for i := 0 to round(abs(L.Number))-1 do
       begin
         s := s + R.Str
       end;
       R.Str := s;
       FFrames.Stack.Push(R);
     end;

     //code duplication

 except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Print;
var
  value : pValue;
begin
  Log('Print');
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_PRINT));
  //value := FFrames.Stack.Pop;
  value := FFrames.Stack.Pop;
   
  FResults.Add('PRINT:' + Value.ToString);
end;




function TVirtualMachine.Call(const Func : pLoxfunction; const ArgCount : integer) : boolean;
begin
  result := false;

  if not (argCount = func.Arity) then
      raise exception.create('param mismatch');

  FFrame := FFrames.Add(Func);

  Result := True;

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
    value := Callee.NativeFunction.Native(ArgCount,FFrames.Stack);
    FFrames.Stack.Push(value);
    result := true;

  end;

end;



procedure TVirtualMachine.DoCall;
var
  ArgCount : byte;
  callee : pValue;
begin
  ArgCount := FFrames.Frame.InstructionPointer.Next^; //read byte
  callee := FFrames.Stack.peek(ArgCount);
  if not callValue(Callee,ArgCount) then
  begin
     raise exception.create('do call mismatch');
  end;
end;


procedure TVirtualMachine.Loop;
var
  a,b : pbyte;
  offset : integer;
begin
   a := FFrames.Frame.InstructionPointer.Next;
   b := FFrames.Frame.InstructionPointer.Next;
   offset := a^ shl 8 + b^;
   assert(FFrames.Frame.InstructionPointer.Move(FFrames.Frame.InstructionPointer.Index - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.Jump;
var
  a,b : pbyte;
  offset : integer;
begin
  a := FFrames.Frame.InstructionPointer.Next;
  b := FFrames.Frame.InstructionPointer.Next;
  offset := a^ shl 8 + b^;
  assert(FFrames.Frame.InstructionPointer.Move(FFrames.Frame.InstructionPointer.Index + offset) = true, 'failed to move to jump offset');
end;

procedure TVirtualMachine.JumpFalse;
var
  a,b : pbyte;
  offset : integer;
begin
   Log('JumpFalse');
   a := FFrames.Frame.InstructionPointer.Next;
   b := FFrames.Frame.InstructionPointer.Next;
   assert(a <> nil, 'a is nil can''t jump');
   assert(b <> nil, 'b is nil can''t jump');
   offset := a^ shl 8 + b^;
   if (isFalsey(FFrames.Stack.peek(0))) then
   begin
     assert(FFrames.Frame.InstructionPointer.Move(FFrames.Frame.InstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.Negate;
var
  R : pValue;
begin
  Log('Negate');
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_NEGATE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    //Assert(R.Operation = OP_CONSTANT); //??

    if (R.IsNumber) then
    begin
      R.Number := - R.Number;
      FFrames.Stack.Push(R);             // note in crafting interpreters, to optimise this you could just negate the actual value without pushing and popping, I think).
    end;



  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.Divide;
var
  L,R : pValue;
begin
  Log('Divide');
  //we assume here we're sitting on an OP_DIVIDE in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_DIVIDE));
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    Assert(R.Number <> 0); //divide by zero exceptions.
    L := FFrames.Stack.Pop;
    L.Number := L.Number / R.Number;
    FFrames.Stack.Push(L);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;

Function TVirtualMachine.isFalsey(value : pValue) : Boolean;
begin
   Log('isFalsey');
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
  Result : pValue;
  
begin
  Log('NotEqual');
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_NOT), 'Current instruction is <> NOT');
  try
    result := NewBool(isFalsey(FFrames.Stack.pop));
    FFrames.Stack.Push(Result);
    FStackResults.Add(Result);
   except on E:exception do
     HandleRunTimeError(e);
  end;
end;

procedure TVirtualMachine.Equal;
var
  L,R, Result : pValue;
begin
   Log('DoEqual');
  //we assume here we're sitting on an OP_EQUAL in the IP
  Assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_EQUAL), 'Current Instruction is <> EQUAL');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FFrames.Stack.Pop;
    L := FFrames.Stack.Pop;

    Result := NewBool(r.ToString = l.ToString);
    FFrames.Stack.Push(result);
    FStackResults.Add(Result);
  except on E:exception do
     HandleRunTimeError(e);
  end;
end;


procedure TVirtualMachine.DoTrue;
var
  value : pValue;
begin
  Log('DoTrue');
  value := newBool(true);
  FFrames.Stack.Push(value);
end;


procedure TVirtualMachine.DoFalse;
var
  Value : pValue;
begin
  Log('DoFalse');
  Value := NewBool(False);
  FFrames.Stack.Push(Value);
end;

procedure TVirtualMachine.DoNil;
var
  value : pValue;
begin
  Log('DoNil');
  new(Value);
  Value.Null := true;
  FFrames.Stack.Push(value);
end;



procedure TVirtualMachine.DoSetLocal;
var
  slot : Integer;
begin
  Log('DoSetLocal');
  assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_Set_LOCAL), 'current instruction is not op define global');
  moveNext;
  Slot := FFrames.Frame.InstructionPointer.CurrentByte^;
  FFrames.Stack[slot] := FFrames.Stack.peek(0);
end;

procedure TVirtualMachine.DoGetLocal;
var
  Slot  : Integer;
  Value : pValue;
  Count : Integer;

begin
  Log('DoGetLocal');
  assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_Get_LOCAL), 'current instruction is not op define global');
  moveNext;
  Slot := FFrames.Frame.InstructionPointer.CurrentByte^;
 //  value := FFrames.Stack.peek(FFrames.Stack.Count-1-PeekIdx);
  value := FFrames.Stack.Item[slot];
  //Log(format('fetched value %s from stack for local using idx %d',[value.tostring, slot]));
  FFrames.Stack.Push(value);

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
var
  name  : pValue;
  value : pValue;
  i : integer;
begin
  //built in natives here
  name := newString('foo');
  AddGlobal(name,NewNative(foo));


  name := newString('DateTime');
  AddGlobal(name,NewNative(DateTime));


  //any others from the list

end;

procedure TVirtualMachine.DoGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   NameValue : pNameValue;

begin
  Log('DoGetGlobal');
  assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_Get_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := FFrames.Frame.InstructionPointer.CurrentByte^;
  name := FFrames.Frame.InstructionPointer.global[constantIndex];
  NameValue := FGlobals.Find(name.tostring);
  Assert(NameValue <> nil, 'expected value does not exist in globals');
  FFrames.Stack.push(NameValue.Value);

  //FFrames.Frame.InstructionPointer.Next;
end;


procedure TVirtualMachine.DoSetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;
   NameValue : pNameValue;
  // bcode : pByteCode;
begin
  Log('DoSetGlobal');
  assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;
  constantIndex := FFrames.Frame.InstructionPointer.CurrentByte^;
  name := FFrames.Frame.InstructionPointer.global[constantIndex];
  value := FFrames.Stack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  NameValue := FGlobals.Find(name.tostring);
  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');
  NameValue.Value := Value;
end;



procedure TVirtualMachine.DoPOP;
begin
  Log('Pop');
  FFrames.Stack.pop;
end;


procedure TVirtualMachine.AddGlobal(const name : pValue ; const Value : pValue);
begin
  Log('AddGlobal');
  assert(assigned(FGlobals.AddNameValue(Name,value)), 'failed to add to hash table');
end;

procedure TVirtualMachine.ClearLog;
begin

end;

procedure TVirtualMachine.Log(Const txt : string);
begin

end;

procedure TVirtualMachine.MoveNext;
begin
  Log('MoveNext');
  Assert(FFrames.Frame.InstructionPointer.Next <> nil,'Expected constant value following constant operation');
end;

function TVirtualMachine.PopStack : pValue;
begin
  Log('PopStack');
  result := FFrames.Stack.Pop;
end;

procedure TVirtualMachine.DoGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;

begin
  Log('Entering DoGlobal');
  assert(FFrames.Frame.InstructionPointer.CurrentByte^ = byte(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := FFrames.Frame.InstructionPointer.CurrentByte^;
  name := FFrames.Frame.InstructionPointer.global[constantIndex];
  value := FFrames.Stack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  AddGlobal(name,value);
  popStack;
  Log('Exiting DoGlobal');
end;

 

procedure TVirtualMachine.DoReturn;
var
  result : pValue;
begin
    //showmessage('entered emit return');

    // Get the result and decrement frame count
    result := popStack;


    // Check if this is the top-level frame
    if FFrames.Count = 0 then
    begin
      // Handle the top-level frame
      popStack;
      //Exit(INTERPRET_OK); what goes here?
    end
    else
    begin
       FFrames.Stack.Push(result);

       FFrames.Remove(FFrames.Frame);

    end;
end;






procedure TVirtualMachine.init(
  //const Natives     : TNatives;
  const Constants   : TValueList;
  const Globals     : TValueList;
  const LoxFunction : pLoxFunction;
  const results : TStrings);
var
  Value : pValue;
begin


  assert(Assigned(results),'No way to display results as no string storage passed in');

  FResults := results;

  FGlobals.Init;

  RegisterNatives;



  FStackResults := TValueList.Create(true);

  FFrames := TCallFrames.create(Constants,Globals);

  call(loxfunction, 0);

end;


procedure TVirtualMachine.finalize;
begin
  FFrames.Stack.Free;
  FGlobals.Finalize;
  FStackResults.free;
end;




end.
