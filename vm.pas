unit vm;

interface

uses
  sysutils,classes,Chunk,Stacks,
  LOXTypes, Table, ValueList, objectFunction;

const
   Frames_Max = 128;
   Stack_Max = Frames_Max * 256;


type
  TInterpretResult = (INTERPRET_NONE,INTERPRET_OK,INTERPRET_COMPILE_ERROR,INTERPRET_RUNTIME_ERROR);


  TFrames = array[0..Frames_Max-1] of TCallFrame;


  TVirtualMachine = record
  private
    FLog : TStrings;
    FFrameCount : integer;
    FFrames : TFrames;



    FInstructionPointer  : TInstructionPointer; //pointer to instructions
    FStackResults : TValueList;  //keep track of new values added to stack.For disposal later.
    FStack  : TValueStack; //byte code stack
    FGlobals : TValuePairs;




    FResults : TStrings;
    procedure ClearLog;
    procedure Log(Const txt : string);
    procedure PopStack;
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
    procedure DoSetGlobal;
    procedure DoPOP;
    procedure DoGetLocal;
    procedure DoSetLocal;
    procedure HandleRunTimeError(const E: Exception);
    procedure JumpFalse;
    procedure Jump;
    Function isFalsey(value : pValue) : Boolean;
    procedure AddGlobal(const name : pValue ; const Value : pValue);

  public
//    function Result : TByteCode;
    function Run : TInterpretResult;
    procedure init(const IP : TInstructionPointer; const results : TStrings; const log : TStrings);
    procedure finalize;
  end;

implementation

uses
  dialogs, addition;


{ TVirtualMachine }
(*function TVirtualMachine.Result: TByteCode;
begin
  assert(FStack.Count = 1, 'stack is empty');
  result := FStack.Pop;
end;*)

function TVirtualMachine.Run : TInterpretResult;
begin
  
  //CallFrame* frame = &vm.frames[vm.frameCount - 1];

  clearLog;

  Log('Run');
  Result := INTERPRET_NONE;
  if FInstructionPointer.ByteCount = 0 then exit;
  while FInstructionPointer.Next <> nil do
  begin

    //ByteCode.Operation := TOpCodes(FInstructionPointer.Current^);

    Case TOpCodes(FInstructionPointer.Current^) of

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
   constantIndex := FInstructionPointer.Current^;
   value := FInstructionPointer.value(constantIndex);
   FStack.Push(value);
end;


procedure TVirtualMachine.Greater;
var
  L,R,Result : pValue;
begin
  Log('greater');
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
  Log('less');
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

procedure TVirtualMachine.DoAdd;
var
  L,R : pValue;
  Result : pValue;
begin
  result := nil;
  Log('Add');
  //we assume here we're sitting on an OP_ADDITION in the IP
  Assert(FInstructionPointer.Current^ = byte(OP_ADD),'Trying to Add - Stack pointer is not OP_ADD');
  //this also means we assume the correct values are sitting in Stack...
  try
    R := FStack.Pop;
    L := FStack.Pop;

    result := TAddition.Add(L,R);
    if assigned(result) then FStack.Push(result);

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

    if (L.IsString) and (R.IsString) then
    begin
      L.Str := SubtractStrings(L.toString,R.ToString);//StringReplace(l.tostring,r.tostring, '', [rfReplaceAll]);    //this is a bit of a curiosity, it doesn't just deduct 1 time, it does it multiple times...
      FStack.Push(L);
      exit;
    end;

    if (L.IsString) and (R.IsNumber) then
    begin
      L.Str := Copy(L.Str,0, Length(L.Str) - round(R.Number));
      FStack.Push(L);
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

     if (L.IsString) and (R.IsNumber) then
     begin
       s := '';
       for i := 0 to round(R.Number-1) do
       begin
         s := s + L.Str
       end;
       L.Str := s;
       FStack.Push(L);
     end;

     if (R.IsString) and (L.IsNumber) then
     begin
       s := '';
       for i := 0 to round(abs(L.Number))-1 do
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
  Log('Print');
  Assert(FInstructionPointer.Current^ = byte(OP_PRINT));
  FResults.Add('PRINT:' + FStack.Pop.ToString);
end;

procedure TVirtualMachine.Loop;
var
  a,b : pbyte;
  offset : integer;
begin
   a := FInstructionPointer.Next;
   b := FInstructionPointer.Next;
   offset := a^ shl 8 + b^;
   assert(FInstructionPointer.Move(FInstructionPointer.Index - offset) = true, 'failed to move to loop offset');
end;


procedure TVirtualMachine.Jump;
var
  a,b : pbyte;
  offset : integer;
begin
  a := FInstructionPointer.Next;
  b := FInstructionPointer.Next;
  offset := a^ shl 8 + b^;
  assert(FInstructionPointer.Move(FInstructionPointer.Index + offset) = true, 'failed to move to jump offset');
end;

procedure TVirtualMachine.JumpFalse;
var
  a,b : pbyte;
  offset : integer;
begin
   Log('JumpFalse');
   a := FInstructionPointer.Next;
   b := FInstructionPointer.Next;
   assert(a <> nil, 'a is nil can''t jump');
   assert(b <> nil, 'b is nil can''t jump');
   offset := a^ shl 8 + b^;
   if (isFalsey(FStack.peek(0))) then
   begin
     assert(FInstructionPointer.Move(FInstructionPointer.Index + offset) = true, 'failed to move to jump false offset');
   end;
end;

procedure TVirtualMachine.Negate;
var
  R : pValue;
begin
  Log('Negate');
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
  Log('Divide');
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
   Log('DoEqual');
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
  Log('DoTrue');
  value := newBool(true);
  FStack.Push(value);
end;


procedure TVirtualMachine.DoFalse;
var
  Value : pValue;
begin
  Log('DoFalse');
  Value := NewBool(False);
  FStack.Push(Value);
end;

procedure TVirtualMachine.DoNil;
var
  value : pValue;
begin
  Log('DoNil');
  new(Value);
  Value.Null := true;
  FStack.Push(value);
end;



procedure TVirtualMachine.DoSetLocal;
var
  slot : Integer;
begin
  Log('DoSetLocal');
  assert(FInstructionPointer.Current^ = byte(OP_Set_LOCAL), 'current instruction is not op define global');
  moveNext;
  Slot := FInstructionPointer.Current^;
  FStack[slot] := FStack.peek(0);
end;

procedure TVirtualMachine.DoGetLocal;
var
  Slot  : Integer;
  Value : pValue;
  Count : Integer;

begin
  Log('DoGetLocal');
  assert(FInstructionPointer.Current^ = byte(OP_Get_LOCAL), 'current instruction is not op define global');
  moveNext;
  Slot := FInstructionPointer.Current^;
 //  value := FStack.peek(FStack.Count-1-PeekIdx);
  value := FStack.Item[slot];
  //Log(format('fetched value %s from stack for local using idx %d',[value.tostring, slot]));
  FStack.Push(value);

end;

procedure TVirtualMachine.DoGetGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   NameValue : pNameValue;

begin
  Log('DoGetGlobal');
  assert(FInstructionPointer.Current^ = byte(OP_Get_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := FInstructionPointer.Current^;
  name := FInstructionPointer.Value(constantIndex);
  NameValue := FGlobals.Find(name.tostring);
  Assert(NameValue <> nil, 'expected value does not exist in globals');
  FStack.push(NameValue.Value);

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
  Log('DoSetGlobal');
  assert(FInstructionPointer.Current^ = byte(OP_SET_GLOBAL), 'current instruction is not op set global');

  MoveNext;
  constantIndex := FInstructionPointer.Current^;
  name := FInstructionPointer.Value(constantIndex);
  value := FStack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  NameValue := FGlobals.Find(name.tostring);
  assert(NameValue <> nil, 'Could not locate global in entries to set its new value');
  NameValue.Value := Value;
end;



procedure TVirtualMachine.DoPOP;
begin
  Log('Pop');
  FStack.pop;
end;


procedure TVirtualMachine.AddGlobal(const name : pValue ; const Value : pValue);
begin
  Log('AddGlobal');
  assert(assigned(FGlobals.AddNameValue(Name,value)), 'failed to add to hash table');
end;

procedure TVirtualMachine.ClearLog;
begin
  FLog.Clear;
end;

procedure TVirtualMachine.Log(Const txt : string);
begin
  FLog.Add(txt);
end;

procedure TVirtualMachine.MoveNext;
begin
  Log('MoveNext');
  Assert(FInstructionPointer.Next <> nil,'Expected constant value following constant operation');
end;

procedure TVirtualMachine.PopStack;
begin
  Log('PopStack');
  FStack.Pop;
end;

procedure TVirtualMachine.DoGlobal;
var
   ConstantIndex : integer;
   Name   : pValue;
   value  : pValue;

begin
  Log('Entering DoGlobal');
  assert(FInstructionPointer.Current^ = byte(OP_DEFINE_GLOBAL), 'current instruction is not op define global');
  MoveNext;
  constantIndex := FInstructionPointer.Current^;
  name := FInstructionPointer.Value(constantIndex);
  value := FStack.Peek(0);
  assert(name.IsString, 'name is not a string object');
  AddGlobal(name,value);
  popStack;
  Log('Exiting DoGlobal');
end;



procedure TVirtualMachine.init(const IP : TInstructionPointer; const results : TStrings; const log : TStrings);
begin
  assert(Assigned(results),'No way to display results as no string storage passed in');
  assert(assigned(log), 'no log for vm');
  FLog := log;
  FLog.clear;
  FResults := results;
  FInstructionPointer := IP;
  FStack.Init;
  FGlobals.Init;
  FStackResults.Init(true);

  //frames
  FFrameCount := 0;

(*   We’ve moved ip out of the VM struct and into
     CallFrame. We need to fix every line of code in the VM that touches ip to
     handle that. Also, the instructions that access local variables by stack slot
     need to be updated to do so relative to the current CallFrame’s slots field. *)
end;


procedure TVirtualMachine.finalize;
begin
  FStack.Finalize;
  FGlobals.Finalize;
  FStackResults.Finalize;
end;




end.
