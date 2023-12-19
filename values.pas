unit Values;

interface

uses classes, LoxTypes, OpCodes;




type

  TNumber = Double;

  TRequester = (rCompiler,rVM);

  pValueRecord = ^TValueRecord;
  TValueRecord = record
    //requester : TRequester;
    case Kind: TLoxKind of
      lxBoolean      :   (Bool    : Boolean);
      lxNumber       :   (Number  : TNumber);
      lxObject       :   (Obj     : Pointer);
  end;

//  TValueStack = class;
  TStack = class;

  TNativeFunction = function(const ArgCount: Integer; const Values : TStack): pValueRecord;

  TNatives = class
  private
    FItems : TList;
  protected
    function getNative(const index : integer) : TNativeFunction;
  public
    procedure Add(const Native : TNativeFunction);

    constructor create;
    destructor destroy; override;
  end;


  PLoxNative = ^TLoxNative;
  TLoxNative = record
    LoxObject : TLoxObject;
    Native    : TNativeFunction;
  end;

  TEightBytes = array[0..7] of byte;



  TOnChunk = procedure(const Operand : TOpCodes) of object;

  TCodes = array of integer;

  TOpCode = class
  const OP_CODE_MULTIPLIER = 8;
  private
    FCodes : TCodes;
    FCount : integer;
    FCapacity : integer;
    procedure GrowCapacity;
    function getCapacity : integer;
    function getCode(const index: integer): integer;
    procedure setCode(const index, Value: integer);
    function getCount: integer;
  protected

  public
    function Add(const value : integer) : integer;
    constructor create;
    destructor destroy; override;
    property Code[const index : integer] : integer read getCode write setCode; default;
    property Count : integer read getCount;
  end;


  TChunks = class
  private
    FOnChunk   : TOnChunk;
    FCode      : TOpCode;
    FConstants : TStack;
    function getCode(const index: integer): integer;
    procedure setCode(const index, Value: integer);
    function getConstant(const index: integer): TValueRecord;

    function getConstantCount: integer;
    function GetCodeCount : Integer;
  public
     procedure Emit(const Operand : TOpCodes; const  value : Integer);overload;
     procedure Emit(const Value : Integer); overload;
     procedure Emit(const Operand : TOpCodes);overload;

     function AddConstant(const value : TValueRecord) : integer;
     procedure EmitConstant(const value : TValueRecord);

     constructor Create(const FunctionName : String);
     destructor destroy;override;

     property Code[const index : integer] : integer read getCode write setCode; default;
     property Constant[const index : integer] : TValueRecord read getConstant;

     property ConstantCount : integer read getConstantCount;
     property CodeCount : integer read getCodeCount;
  end;

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
    LoxObject : TLoxObject;
    FuncKind  : TFunctionKind;
    Arity     : integer; // The arity field stores the number of parameters the function expects.
    Name      : String;
    Chunks    : TChunks;
  end;

  pLoxList = ^TLoxList;


  TLoxList = record
     LoxObject : TLoxObject;
     Name      : string;
     Items     : TStack;
  end;

  TOnStackPush = procedure(Const stack : TStack) of object;
  TOnStackPop  = procedure(Const stack : TStack) of object;


  TBlock_Capacity = 8..256;

  TStack = class
  const STACK_MAX = 4 * high(TBlock_Capacity);
  private
    FOnStackPush : TOnStackPush;
    FOnStackPop  : TOnStackPop;
    FCapacity : integer;
    FStackTop : integer;
    FItems :  array of TValueRecord;
    function  GetItem(const Index: integer): TValueRecord;
    procedure SetItem(const Index: integer; const Value: TValueRecord);
    procedure SetOnStackPop(const Value: TOnStackPop);
    procedure SetOnStackPush(const Value: TOnStackPush);
  protected
     procedure IncreaseCapacity;
     procedure SetStackTop(const value : integer);
  public
     procedure Push(const value : TValueRecord);
     function Pop : TValueRecord;
     function Peek(const Distance : integer) : TValueRecord;overload;
     function Peek : TValueRecord; overload;
     constructor Create(const Block_Capacity : TBlock_Capacity = 256);
     destructor Destroy;override;
     property Capacity : integer read FCapacity;
     property Item[Const Index : integer] : TValueRecord read GetItem write SetItem; default;
     property StackTop : integer read FStackTop write SetStackTop;
     property OnPush : TOnStackPush read FOnStackPush write SetOnStackPush;
     property OnPop : TOnStackPop read FOnStackPop write SetOnStackPop;
  end;

  TInstructionPointer = record
  private
    FFunction : PLoxFunction;
    FIndex    : integer;
    function getCount: integer;
    function Getconstant(const Index : integer) : TValueRecord;
    function getFunction: PLoxFunction;
    procedure setFunction(const Value: PLoxFunction);
    function GetIndex : integer;
    function GetValue(const Index : integer) : Integer;

    procedure setIndex(const Value: integer);
  public
    function Move(const index : integer) : boolean;


    function Current : Integer;
    function Next : Integer;
   // constructor create;
//    destructor destroy; override;
    //constructor create(
    //    const loxFunction : PLoxFunction);
    property count : integer read getCount;
    property Index : integer read getIndex write setIndex;
    property constant[const index : integer] : TValueRecord read getConstant;
    property code[const index : integer] :  integer read GetValue; Default;
    property Func : PLoxFunction read getFunction write setFunction;
  end;

 TCallFrames = class;



 TCallFrame = class
 private
   FPrevOPCode         : integer;
   FStackTop           : integer;
   FFunction           : pLoxFunction;
  // FInstructionPointer : TInstructionPointer;
   FStack              : TStack;

   function  getValue(const index: integer): TValueRecord;
   procedure setValue(const index: integer; const Value: TValueRecord);
 public
    constructor create(
      const PrevOpCode : Integer;
      const Func  : PLoxFunction;
      const Stack : TStack;
      const StackTop : integer);
   destructor destroy;override;
//   property InstructionPointer : TInstructionPointer read FInstructionPointer;
   property Func : pLoxFunction read FFunction;
   property Value[const index : integer] : TValueRecord read getValue write setValue;default;
   property StackTop : integer read FStackTop write FStackTop;
   property PrevOpCode : integer read FPrevOpCode;

 end;

  TCallFrameList = Array of TCallFrame;

  TFrameStack = class
  const Frame_Stack_Mulitplier = 255;
  private
    FCapacity   : integer;
    FStackTop  : integer; //note this points the next places a frame will go
    FItems :  TCallFrameList;
    function getItem(const index: integer): TCallFrame;
    function getStackTop: integer;
    procedure setItem(const index: integer; const Value: TCallFrame);
    procedure setStackTop(const Value: integer);

  protected
     procedure IncreaseCapacity;
     function GetCapacity : integer;
  public
    function peek : TCallFrame; overload;
    function Peek(const fromTop : integer) : TCallFrame; overload;
    procedure Push(const value : TCallFrame);
    function pop : TCallFrame;
    constructor create;
    destructor destroy;override;
    property StackTop : integer read getStackTop write setStackTop;
    property Capacity : integer read getCapacity;
    property Item[const index : integer] : TCallFrame read getItem write setItem; default;
  end;

  TCallFrames = class
  private
    FFrames : TFrameStack;
  protected
  public
    constructor create;
    destructor destroy; override;
    procedure Push(const CallFrame : TCallFrame);
    function Pop : TCallFrame;
    function Peek : TCallFrame;
    function StackTop : integer;
  end;



implementation

uses
  sysutils,
  dialogs,
  Exceptions,
  ValueManager;

 

 
function TInstructionPointer.GetCount: integer;
begin
  result := FFunction.Chunks.CodeCount;
end;

function TInstructionPointer.getFunction: PLoxFunction;
begin
  result := FFunction;
end;



function TInstructionPointer.GetValue(const Index : integer) : integer;
begin
  assert((index >= 0) and (index < FFunction.Chunks.CodeCount));
  result := FFunction.Chunks[FIndex];
end;

(*constructor TInstructionPointer.create;
begin
  fIndex := -1;
end; *)

function TInstructionPointer.Current: integer;
begin
  result := getValue(FIndex);
end;

(*
destructor TInstructionPointer.destroy;
begin
  FFunction := nil;
  inherited destroy;
end; *)

function TInstructionPointer.Getconstant(const Index: integer): TValueRecord;
begin
  result := FFunction.Chunks.Constant[Index];
end;

function TInstructionPointer.getIndex: integer;
begin
  result := FIndex;
end;

 
function TInstructionPointer.Move(const index: integer): boolean;
begin
  result := false;
  if (index >= 0) and (index < FFunction.Chunks.CodeCount) then
  begin
    FIndex := index;
    result := true;
  end;
end;


function TInstructionPointer.Next: integer;
begin
  result := -1;
  if FFunction.Chunks.CodeCount = 0 then exit;

  inc(FIndex);
  if FIndex = FFunction.Chunks.CodeCount then exit;
  result := Current;
end;


procedure TInstructionPointer.setFunction(const Value: PLoxFunction);
begin
  FFunction := Value;
end;

procedure TInstructionPointer.setIndex(const Value: integer);
begin
  Assert(FFunction.Chunks.CodeCount > 0, 'can''t set index as there is no opcodes');
  Assert(Value >= -1, 'opcode index is less than -1');
  Assert(Value < FFunction.Chunks.CodeCount, 'op code index is > FFunction.Chunks.CodeCount');
  FIndex := Value;
end;

procedure TCallFrames.Push(const CallFrame : TCallFrame);
begin
  FFrames.Push(CallFrame);
end;


function TCallFrames.StackTop: integer;
begin
  result := FFrames.StackTop;
end;

constructor TCallFrames.create;
begin
  FFrames := TFrameStack.Create;//Tlist.create;
end;

destructor TCallFrames.destroy;
begin
  FFrames.Free;                      
end;


function TCallFrames.Peek: TCallFrame;
begin
  result := nil;
  if FFrames.StackTop > 0 then
    result := FFrames[FFrames.StackTop-1];
end;



function TCallFrames.Pop : TCallFrame;
begin
  result := FFrames.Pop;
end;

{ TCallFrame }

(*
procedure TCallFrame.InitInstructionPointer(const func : pLoxFunction);
begin

  FInstructionPointer.Func := func;
end;  *)

constructor TCallFrame.create(
  const PrevOpCode : Integer;
  const Func  : pLoxFunction;
  const Stack : TStack;
  const StackTop : integer);
begin
  Assert(Assigned(Stack),'no stack injected');
//  FInstructionPointer := InstructionPointer;
  FPrevOpCode := PrevOpCode;
  FFunction := Func;
  FStack := Stack;
  FStackTop := StackTop;
end;

destructor TCallFrame.destroy;
begin


  inherited;
end;

(*function TCallFrame.getStackCount: integer;
begin
  result := FStack.Count;
end; *)
 
function TCallFrame.getValue(const index: integer): TValueRecord;
begin
  result := FStack[FStackTop + index];
end;

procedure TCallFrame.setValue(const index: integer; const Value: TValueRecord);
begin
   FStack[FStackTop + index] := Value;
end;

{ TNatives }

procedure TNatives.Add(const Native: TNativeFunction);
begin
  FItems.Add(@Native);
end;

constructor TNatives.create;
begin
  FItems := TList.create;
end;

destructor TNatives.destroy;
begin
  FItems.Free;
  inherited;
end;

function TNatives.getNative(const index: integer): TNativeFunction;
begin
  result := FItems[index];
end;


procedure TChunks.Emit(const Operand : TOpCodes);
begin
  FCode.Add(ord(Operand));
end;

procedure TChunks.Emit(const value : integer);
begin
  FCode.Add(value);
end;


procedure TChunks.Emit(const Operand : TOpCodes; const  value : integer);
begin
  emit(Operand);
  emit(Value);
end;

function TChunks.getCode(const index: integer): integer;
begin
  result := FCode[index];
end;

function TChunks.getConstant(const index: integer): TValueRecord;
begin
  result := FConstants[Index];
end;

function TChunks.getConstantCount: integer;
begin
  result := FConstants.StackTop;
end;


procedure TChunks.setCode(const index, Value: integer);
begin
  FCode[Index] := Value;
end;


function TChunks.AddConstant(const value: TValueRecord): integer;
begin
  FConstants.Push(Value);
  result := FConstants.StackTop-1;
end;


procedure TChunks.EmitConstant(const value : TValueRecord);
begin
  FConstants.Push(value);
  Emit(OP_CONSTANT,FConstants.StackTop-1);
end;


function TChunks.GetCodeCount: Integer;
begin
  result := FCode.Count;
end;

constructor TChunks.Create;
begin
  FCode := TOpCode.Create;
  FConstants := TStack.Create(8);
end;

destructor TChunks.destroy;
begin
  FCode.Free;
  FConstants.Free;
  inherited;
end;


{ TOpCodes }

function TOpCode.Add(const value: integer) : integer;
begin
  result := FCount;

  if FCount = FCapacity then
    growCapacity;

  fCodes[fCount] := Value;
  inc(FCount);
end;

constructor TOpCode.create;
begin
  FCount := 0;
  FCapacity :=  OP_CODE_MULTIPLIER;
  SetLength(FCodes, FCapacity);
end;

destructor TOpCode.destroy;
begin

  inherited;
end;

function TOpCode.getCapacity: integer;
begin
  result := FCapacity;
end;

function TOpCode.getCode(const index: integer): integer;
begin
   result := FCodes[Index];
end;

function TOpCode.getCount: integer;
begin
  result := FCount;
end;

procedure TOpCode.GrowCapacity;
begin
  FCapacity := FCapacity * 2;
  setLength(FCodes,FCapacity);
end;

procedure TOpCode.setCode(const index, Value: integer);
begin
  assert(index <= FCapacity, 'Out of bounds index opcodes');
  FCodes[index] := value;
end;



{ TStack }

constructor TFrameStack.create;
begin
  FStackTop := 0;
  FCapacity :=  Frame_Stack_Mulitplier;
  setLength(FItems,FCapacity);
end;

destructor TFrameStack.destroy;
begin
  inherited;
end;

function TFrameStack.GetCapacity: integer;
begin
  result := FCapacity;
end;

function TFrameStack.getItem(const index: integer): TCallFrame;
begin
  result := FItems[index];
end;

function TFrameStack.getStackTop: integer;
begin
  result := FStackTop;
end;

procedure TFrameStack.IncreaseCapacity;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems,FCapacity);
end;

function TFrameStack.peek(const fromTop: integer): TCallFrame;
begin
  assert(FromTop >= 0, 'This is distance from the top as a positive');
  assert(FStackTop - FromTop >= 0, 'Distance is beyond stack bottom');
  result := FItems[FStackTop-FromTop-1];
end;

function TFrameStack.peek: TCallFrame;
begin
   result := FItems[FStackTop-1];
end;

function TFrameStack.pop: TCallFrame;
begin
  assert(FStackTop >0, 'No items to pop');
  result := FItems[FStackTop-1];
  FItems[FStackTop-1] := nil;
  FStackTop := FStackTop - 1;
end;

procedure TFrameStack.Push(const value: TCallFrame);
begin
  FItems[FStackTop] := value;
  SetStackTop(FStackTop+1);
end;

procedure TFrameStack.setItem(const index: integer; const Value: TCallFrame);
begin
  assert(index >= 0, 'index is negative to stack bottom');
  assert(index <= FStackTop, 'index is beyond stack top');
  FItems[index] := Value;
end;

procedure TFrameStack.setStackTop(const Value: integer);
begin
  if Value > FCapacity-1 then   //since the FStackTop starts at 0
  begin
    IncreaseCapacity; 
   end;
   FStackTop := Value;
end;

{ TStack }

constructor TStack.Create(const Block_Capacity : TBlock_Capacity=256);
begin
  FCapacity := Block_Capacity;
  SetLength(FItems,FCapacity);
  FStackTop := 0;
end;


destructor TStack.Destroy;
begin
  inherited;
end;

function TStack.GetItem(const Index: integer): TValueRecord;
begin
  assert(Index < FCapacity, 'out of bounds stack get');
  result := FItems[Index];
end;

procedure TStack.IncreaseCapacity;
begin
  FCapacity := FCapacity  * 2;
  SetLength(FItems,FCapacity);
end;

function TStack.Peek: TValueRecord;
begin
  assert(FStackTop > 0, 'Nothing on the stack');
  result := FItems[FStackTop-1];
end;

function TStack.Peek(const Distance: integer): TValueRecord;
begin
  assert(FStackTop > 0, 'Nothing on the stack');
  assert(Distance >= 0, 'This is distance from the top as a positive');
  assert(FStackTop - Distance >= 0, 'Distance is beyond stack bottom');
  result := FItems[FStackTop-Distance-1];
end;

function TStack.Pop: TValueRecord;
begin
  Assert(FStackTop > 0, 'no more items to pop');
  result := FItems[FStackTop-1];
  SetStackTop(FStackTop-1);
 // if Assigned(FOnStackPop) then FOnStackPop(Self);
end;

procedure TStack.Push(const value: TValueRecord);
begin

  FItems[FStackTop] := Value;
  SetStackTop(FStackTop+1);

  //if Assigned(FOnStackPush) then FOnStackPush(Self);
end;

procedure TStack.SetItem(const Index: integer; const Value: TValueRecord);
begin
  assert(Index < FCapacity, 'Index is > Capacity');
  assert(Index >= 0, 'Index is < 0');

  FItems[Index] := Value;
end;

procedure TStack.SetOnStackPop(const Value: TOnStackPop);
begin
  FOnStackPop := Value;
end;

procedure TStack.SetOnStackPush(const Value: TOnStackPush);
begin
  FOnStackPush := Value;
end;

procedure TStack.SetStackTop(const value: integer);
begin
  Assert(Value >= 0, 'You idiot, the stack top is below zero');
  Assert(Value < STACK_MAX, 'trying to set stack top beyond the absolute max');
  while (value >= FCapacity) and (Value < STACK_MAX) do
  begin
    IncreaseCapacity;
  end;

  FStackTop := Value;


end;

end.


