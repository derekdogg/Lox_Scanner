unit Values;

interface

uses classes, LoxTypes, OpCodes;

const
   TShortSize = 8;


type

  TNumber = double;


  TShortStr = string[TShortSize];

  TRequester = (rCompiler,rVM);

  pValueRecord = ^TValueRecord;
  TValueRecord = record
    //requester : TRequester;
    case Kind: TLoxKind of

      lxShort        :   (str     : TShortStr);
      lxNumber       :   (Number  : TNumber);
      lxBoolean      :   (Bool    : Boolean);
      lxObject       :   (Obj     : Pointer);

  end;



  TOnChunk = procedure(const Operand : Integer) of object;

  TOpCodeValue = smallint;

  const OP_CODE_CAPACITY = 256; //keep this at the minimum, because affects patch jumping?? Interesting observation....

type

    TCodes = array of TOpCodeValue;

    TOpCode = record
      Codes : TCodes;
      Count : integer;
      Capacity : integer;
      procedure GrowCapacity;
      function Add(const value : TOpCodeValue) : TOpCodeValue;
      procedure Init;
    end;


  TBlock_Capacity = 8..256;

  StackList = array of TValueRecord;

  TStack = record
     Items :  StackList;
     Capacity : integer;
     StackTop : integer;
     procedure Add;
     procedure Subtract;
     procedure Less;
     function Copy(const source : integer; const dest : integer) : boolean;
     function isFalse : boolean;
     procedure Push(const value : TValueRecord);
     function Pop : TValueRecord;
     function Peek(const Distance : integer) : TValueRecord;overload;
     function Peek : TValueRecord; overload;
     procedure Init(const Block_Capacity : TBlock_Capacity = 256);
  end;

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
    LoxObject  : TLoxObject;
    FuncKind   : TFunctionKind;
    Arity      : byte; // The arity field stores the number of parameters the function expects.
    Name       : string;
    OpCodes    : TOpCode;
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
  OpCodes.Add(value);
end;


procedure TLoxFunction.Emit(const Operand : Integer; const  value : integer);
begin
  emit(Operand);
  emit(Value);
end;

function TLoxFunction.getCode(const index: integer): integer;
begin
  result := OPCodes.Codes[index];
end;

function TLoxFunction.getConstant(const index: integer): TValueRecord;
begin
  result := Constants.Items[Index];
end;

function TLoxFunction.getConstantCount: integer;
begin
  result := Constants.StackTop;
end;


procedure TLoxFunction.setCode(const index, Value: integer);
begin
  opCodes.Codes[Index] := Value;
end;


function TLoxFunction.AddConstant(const value: TValueRecord): integer;
begin
  Constants.Push(Value);
  result := Constants.StackTop-1;
end;


procedure TLoxFunction.EmitConstant(const value : TValueRecord);
begin
  Constants.Push(value);
  Emit(OP_CONSTANT,Constants.StackTop-1);
end;


function TLoxFunction.GetCodeCount: Integer;
begin
  result := OpCodes.Count;
end;

constructor TLoxFunction.Create;
begin
  OpCodes.Init; // := TOpCode.Create;
  Constants.init(8);
end;

(*destructor TLoxFunction.destroy;
begin
  FCode.Free;
//  FConstants.Free;
  inherited;
end; *)

function TOpCode.Add(const value: TOpCodeValue) : TOpCodeValue;
begin
  result := Count;

  if Count = Capacity then
    growCapacity;

  Codes[Count] := Value;
  inc(Count);
end;

procedure TOpCode.Init;
begin
  Count := 0;
  Capacity :=  OP_CODE_CAPACITY;
  SetLength(Codes, Capacity);
end;

procedure TOpCode.GrowCapacity;
begin
  Capacity := Capacity * 2;
  setLength(Codes,Capacity);
end;





{ TStack }

procedure TStack.Init(const Block_Capacity : TBlock_Capacity=256);
begin
  Capacity := Block_Capacity;
  SetLength(Items,Capacity);
  StackTop := 0;
end;

function TStack.Peek: TValueRecord;
begin
  assert(StackTop > 0, 'Nothing on the stack');
  result := Items[StackTop-1];
end;

procedure TStack.Add;
begin
  Items[StackTop-2].Number := Items[StackTop-2].Number + Items[StackTop-1].Number;
  dec(StackTop);
end;

procedure TStack.Subtract;
begin
  Items[StackTop-2].Number := Items[StackTop-2].Number - Items[StackTop-1].Number;
  dec(StackTop);
end;

procedure TStack.Less;
begin
  Items[StackTop-2].Bool := Items[StackTop-2].Number < Items[StackTop-1].Number;
  Items[StackTop-2].Kind := lxBoolean;
  StackTop := StackTop -1;
end;


function TStack.isFalse : boolean;
var
  value : TValueRecord;
begin
  result := false;
  Value := Items[StackTop-1];
  case Value.Kind of
    lxBoolean : result := Value.Bool = false;
    lxNumber  : result := Value.Number <= 0;
    lxString  : result := lowercase(trim(GetString(Value))) = 'false';
    lxNull    : result := true;
  end;
end;

function TStack.Peek(const Distance: integer): TValueRecord;
begin
  (*assert(FStackTop > 0, 'Nothing on the stack');
  assert(Distance >= 0, 'This is distance from the top as a positive');
  assert(FStackTop - Distance >= 0, 'Distance is beyond stack bottom'); *)
  result := Items[StackTop-Distance-1];
end;

function TStack.Pop: TValueRecord;
begin
  Assert(StackTop > 0, 'no more items to pop');
  result := Items[StackTop-1];

  dec(StackTop);
 // if Assigned(FOnStackPop) then FOnStackPop(Self);
end;




procedure TStack.Push(const value: TValueRecord);
begin
  Assert((StackTop + 1) < Capacity, 'out of bounds stack');
  //result := StackTop;

  Items[StackTop] := Value;
  StackTop := StackTop + 1;

  //if Assigned(FOnStackPush) then FOnStackPush(Self);
end;

function TStack.Copy(const source : integer; const dest : integer) : boolean;
begin
  assert(Source < Capacity, 'Index is > Capacity');
  assert(Source >= 0, 'Index is < 0');
  assert(dest < Capacity, 'Index is > Capacity');
  assert(dest >= 0, 'Index is < 0');
  Items[dest] := Items[Source];

end;


end.


