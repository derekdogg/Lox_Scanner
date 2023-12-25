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
      lxObject       :   (Obj     : Pointer);
      lxNumber       :   (Number  : TNumber);
      lxBoolean      :   (Bool    : Boolean);

  end;



  TOnChunk = procedure(const Operand : Integer) of object;


  TOpCode = record
  const OP_CODE_CAPACITY = 256; //keep this at the minimum, because affects patch jumping?? Interesting observation....
  private
    FCodes : array of integer;
    FCount : integer;
    FCapacity : integer;
    procedure GrowCapacity;
    function getCode(const index: integer): integer;
    procedure setCode(const index, Value: integer);
    function getCount: integer;
  public
    function Add(const value : integer) : integer;
    procedure Init;
    property Code[const index : integer] : integer read getCode write setCode; default;
    property Count : integer read getCount;
  end;






  TBlock_Capacity = 8..256;

  StackList = array of TValueRecord;

  TStack = record
  private
    FCapacity : integer;
    FStackTop : integer;
    FItems :  StackList;


     procedure IncreaseCapacity;
     procedure SetStackTop(const value : integer);
  public
     procedure SetItem(const Index: integer; const Value: TValueRecord);
     function  GetItem(const Index: integer): TValueRecord;
     procedure Add;
     procedure Subtract;
     procedure Less;
     function isFalse : boolean;
     function Push(const value : TValueRecord) : integer;
     function Pop : TValueRecord;
     function Peek(const Distance : integer) : TValueRecord;overload;
     function Peek : TValueRecord; overload;
     procedure Init(const Block_Capacity : TBlock_Capacity = 256);
     property Items : StackList read FItems;
     property Capacity : integer read FCapacity;
     property Item[Const Index : integer] : TValueRecord read GetItem write SetItem; default;
     property StackTop : integer read FStackTop write SetStackTop;
  end;

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
    LoxObject  : TLoxObject;
    FuncKind   : TFunctionKind;
    Arity      : byte; // The arity field stores the number of parameters the function expects.
    Name       : String;
    Codes      : TOpCode;
    Constants  : TStack;
    function getCode(const index: integer): integer;
    procedure setCode(const index, Value: integer);
    function getConstant(const index: integer): TValueRecord;
    function getConstantCount: integer;
    function GetCodeCount : Integer;
    procedure Emit(const Operand : Integer; const  value : Integer);overload;
    procedure Emit(const Value : Integer); overload;
    function AddConstant(const value : TValueRecord) : integer;
    procedure EmitConstant(const value : TValueRecord);
    constructor Create(const FunctionName : String);
    
  end;


  pLoxList = ^TLoxList;


  TLoxList = record
     LoxObject : TLoxObject;
     Name      : string;
     Items     : TStack;
  end;


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


  TInstructionPointer = record
  private
    FCodeCount : integer;
    FFunction  : PLoxFunction;
    FIndex     : integer;
    FCodes     : TOpCode;
    FConstants : TStack;
    function Getconstant(const Index : integer) : TValueRecord;
    procedure setFunction(const Value: PLoxFunction);
    function GetValue(const Index : integer) : Integer;
    procedure setIndex(const Value: integer);
  public
    function Move(const index : integer) : boolean;
    function increment(const index : integer) : boolean;
    function Next : Integer;

    property CodeCount : integer read FCodeCount;
    property Index : integer read FIndex write FIndex;
    property constant[const index : integer] : TValueRecord read getConstant;
    property code[const index : integer] :  integer read GetValue; Default;
    property Func : PLoxFunction write setFunction;
  end;


  procedure PrintOpCodes(const fn : TLoxFunction; const destination : TStrings);


implementation

uses
  sysutils,
  dialogs,
  Exceptions,
  ValueManager;

  procedure PrintOpCodes(const fn : TLoxFunction; const destination : TStrings);
  var
    i : integer;
  begin
    //for i := 0 to fn.Code
  end;



function TInstructionPointer.GetValue(const Index : integer) : integer;
begin
  assert((index >= 0) and (index < FCodeCount));
  result := FCodes[FIndex];
end;

function TInstructionPointer.Getconstant(const Index: integer): TValueRecord;
begin
  result := FConstants[Index];
end;


function TInstructionPointer.increment(const index : integer) : boolean;
begin
  result := Move(FIndex + index);
end;

function TInstructionPointer.Move(const index: integer): boolean;
begin
  result := false;
  if (index >= 0) and (index < FCodeCount) then
  begin
    FIndex := index;
    result := true;
  end;
end;


function TInstructionPointer.Next: integer;
begin
  result := -1;

  if FCodeCount = 0 then exit;

  inc(FIndex);

  if FIndex = FCodeCount then exit;

  result :=  FCodes[FIndex];
end;


procedure TInstructionPointer.setFunction(const Value: PLoxFunction);
begin
  FFunction := Value;
  FCodes     := Value.Codes;
  FCodeCount := FCodes.Count;
  FConstants := Value.Constants;
end;

procedure TInstructionPointer.setIndex(const Value: integer);
begin
  Assert(FCodeCount > 0, 'can''t set index as there is no opcodes');
  Assert(Value >= -1, 'opcode index is less than -1');
  Assert(Value < FCodeCount, 'op code index is > FCodeCount');
  FIndex := Value;
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

procedure TLoxFunction.Emit(const value : integer);
begin
  Codes.Add(value);
end;


procedure TLoxFunction.Emit(const Operand : Integer; const  value : integer);
begin
  emit(Operand);
  emit(Value);
end;

function TLoxFunction.getCode(const index: integer): integer;
begin
  result := Codes[index];
end;

function TLoxFunction.getConstant(const index: integer): TValueRecord;
begin
  result := Constants[Index];
end;

function TLoxFunction.getConstantCount: integer;
begin
  result := Constants.StackTop;
end;


procedure TLoxFunction.setCode(const index, Value: integer);
begin
  Codes[Index] := Value;
end;


function TLoxFunction.AddConstant(const value: TValueRecord): integer;
begin
  result := Constants.Push(Value);
end;


procedure TLoxFunction.EmitConstant(const value : TValueRecord);
begin
  Emit(OP_CONSTANT,Constants.Push(value));
end;


function TLoxFunction.GetCodeCount: Integer;
begin
  result := Codes.Count;
end;

constructor TLoxFunction.Create;
begin
  Codes.Init; // := TOpCode.Create;
  Constants.init(8);
end;

(*destructor TLoxFunction.destroy;
begin
  FCode.Free;
//  FConstants.Free;
  inherited;
end; *)

function TOpCode.Add(const value: integer) : integer;
begin
  result := FCount;

  if FCount = FCapacity then
    growCapacity;

  fCodes[fCount] := Value;
  inc(FCount);
end;

procedure TOpCode.Init;
begin
  FCount := 0;
  FCapacity :=  OP_CODE_CAPACITY;
  SetLength(FCodes, FCapacity);
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

procedure TStack.Init(const Block_Capacity : TBlock_Capacity=256);
begin
  FCapacity := Block_Capacity;
  SetLength(FItems,FCapacity);
  FStackTop := 0;
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

procedure TStack.Add;
begin
  FItems[FStackTop-2].Number := FItems[FStackTop-2].Number + FItems[FStackTop-1].Number;
  FStackTop := FStackTop -1;
end;

procedure TStack.Subtract;
begin
  FItems[FStackTop-2].Number := FItems[FStackTop-2].Number - FItems[FStackTop-1].Number;
  FStackTop := FStackTop -1;
end;

procedure TStack.Less;
begin
  FItems[FStackTop-2].Bool := FItems[FStackTop-2].Number < FItems[FStackTop-1].Number;
  FItems[FStackTop-2].Kind := lxBoolean;
  FStackTop := FStackTop -1;
end;


function TStack.isFalse : boolean;
var
  value : TValueRecord;
begin
  result := false;
  Value := FItems[FStackTop-1];
  case Value.Kind of
    lxBoolean : result := Value.Bool = false;
    lxNumber  : result := Value.Number <= 0;
    lxString  : result := lowercase(trim(GetString(Value))) = 'false';
    lxNull    : result := true;
  end;
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




function TStack.Push(const value: TValueRecord) : integer;
begin
  result := FStackTop;
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


procedure TStack.SetStackTop(const value: integer);
begin
  Assert(Value >= 0, 'You idiot, the stack top is below zero');

  while (value >= FCapacity) do
  begin
    IncreaseCapacity;
  end;

  FStackTop := Value;
 
end;

end.


