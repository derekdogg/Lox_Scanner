unit Arrays;

interface
uses
  sysutils,loxTypes,values, valueManager;

const
  Stack_Capacity = 256;

type

 // pValueRecord = pValueRecord;

  pStackList = ^TStackList;
  TStackList = array[0..Stack_Capacity-1] of TValueRecord;

  TStackRecord = record
    Count     : integer;
    StackList : pStackList;
    Capacity  : integer;
    StackTop  : pValueRecord;
  end;

  function NewStackList : pStackList; 

  procedure DisposeStackList(StackList : pStackList); 

  procedure InitStack(var stack : TStackRecord; const StackList : pStackList);

  function GetReference(stackList : pStackList; const index : integer) : pValueRecord;

  function GetValue(stackList : pStackList; const index : integer) : TValueRecord;

  procedure SetValue(stackList : pStackList; const index : integer; const value : TValueRecord);

  procedure push(var Stack : TStackRecord; const value : TValueRecord);

  function pop(var Stack : TStackRecord) : TValueRecord;

  procedure SetStackTop(var Stack : TStackRecord; Const Index : integer);

  function PeekTop(const Stack : TStackRecord) : TValueRecord;

  function PeekSecond(const Stack : TStackRecord) : TValueRecord;

  function PeekFromTop(const Stack : TStackRecord; const Amount : integer) : TValueRecord;

  function TopIsFalse(const stack : TStackRecord) : boolean;




implementation


  procedure SetStackTop(var Stack : TStackRecord; const index : Integer);
  begin
    assert(Index >= 0, 'top beyond stack limit');
    assert(Index < Stack_Capacity, 'top beyond stack limit');
    Stack.StackTop  := @Stack.StackList[index];
    Stack.Count     := index;
  end;

  procedure InitStack(var stack : TStackRecord; const StackList : pStackList);
  begin
     Stack.StackList := StackList;
     SetStackTop(Stack,0);
     Stack.Capacity := Stack_Capacity;
     //Stack.StackTop  := @StackList[0];
     //Stack.Count     := 0;
  end;

  function GetReference(stackList : pStackList; const index : integer) : pValueRecord;
  begin
    assert(Index >= 0, 'top beyond stack limit');
    assert(Index < Stack_Capacity, 'top beyond stack limit');
    result := @StackList[0];
    inc(result,Index);
  end;

  function GetValue(stackList : pStackList; const index : integer) : TValueRecord;
  begin
    result := GetReference(StackList,Index)^;
  end;

  procedure SetValue(stackList : pStackList; const index : integer; const value : TValueRecord);
  begin
     GetReference(StackList,Index)^ := Value;
  end;
 
  procedure push(var Stack : TStackRecord; const value : TValueRecord);
  begin
    assert(Stack.Count < Stack_Capacity, 'push beyond stack limit');
    Stack.StackTop^ := Value;
    inc(Stack.StackTop);
    inc(Stack.Count);
  end;

  function pop(var Stack : TStackRecord) : TValueRecord;
  begin
    assert(Stack.Count >= 1, 'pop beyond stack limit');
    dec(Stack.StackTop);
    dec(Stack.Count);
    result := Stack.StackTop^;
  end;

  function PeekTop(const Stack : TStackRecord) : TValueRecord;
  begin
    result := GetValue(Stack.stackList,Stack.Count-1);
  end;

  function PeekSecond(const Stack : TStackRecord) : TValueRecord;
  begin
    result := GetValue(Stack.stackList,Stack.Count-2);
  end;

  function PeekFromTop(const Stack : TStackRecord; const Amount : integer) : TValueRecord;
  begin
     result := GetValue(Stack.stackList,Stack.Count-1 - Amount);
  end;

  function TopIsFalse(const stack : TStackRecord) : boolean;
  var
    value : TValueRecord;
  begin
    result := false;
    Value := PeekTop(Stack);
    case Value.Kind of
      lxBoolean : result := Value.Bool = false;
      lxNumber  : result := Value.Number <= 0;
      lxString  : result := lowercase(trim(GetString(Value))) = 'false';
      lxNull    : result := true;
    end;
  end;



  procedure Subtract(stackList : pStackList);
  begin

  end;


  function NewStackList : pStackList;
  begin
    getMem(result,sizeof(TStackList));
    fillChar(result^,sizeof(TStackList),#0);
  end;

  procedure DisposeStackList(StackList : pStackList);
  begin
    freeMem(StackList);
  end;

(*  function StackTop(const StackList : pStackList) : pValueRecord;
  begin

  end; *)



   


end.


 
