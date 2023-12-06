unit Values;

interface

uses classes, LoxTypes, OpCodes;


type

  pValue = ^TValue;

  TValueRecord = record
    case Kind: TLoxKind of
      lxBoolean      :   (Bool: Boolean);
      lxNumber       :   (Number: Double);
      lxObject       :   (Obj: Pointer);


//  TValueStack = class;
  TStack = class;

  TNativeFunction = function(const ArgCount: Integer; const Values : TStack): pValue;

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



  TNumber = Double;

  TValueList = class;


  TOnChunk = procedure(const Operand : TOpCodes) of object;


  TConstants = class
  private
    fName : string;
    FItems : TValueList;
    function getItem(const Index: integer): pValue;
    procedure setItem(const Index: integer; const Value: pValue);
    function getCount: integer;
    function getOwnValues: boolean;
  protected

  public
    constructor create(const functionName : String); //to help with debugging stuff
    destructor destroy; override;
    function Add(const value : pValue) : integer;
    property OwnValues : boolean read getOwnValues;
    property Item[const Index : integer] : pValue read getItem write setItem;default;
    property Count : integer read getCount;
  end;

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

    function Add(const value : integer) : integer;

  public
    constructor create;
    destructor destroy; override;
    property Code[const index : integer] : integer read getCode write setCode; default;
    property Count : integer read getCount;
  end;

  TChunks = class
  private
    //FConstantCount : integer;
    FName : String;
    FOnChunk   : TOnChunk;
    FCode      : TOpCode;
    FConstants : TConstants;
    function getCode(const index: integer): integer;
    procedure setCode(const index, Value: integer);
    function getConstant(const index: integer): pValue;
    procedure setConstant(const index : integer; const Value: pValue);
    function getConstantCount: integer;
    function getOwnsValues: boolean;

  public
     function Emit(const Operand : TOpCodes; const  value : Integer) : integer;overload;
     function Emit(const valueA, ValueB : Integer) : Integer;overload;
     function Emit(const Value : Integer) : integer; overload;
     function Emit(const Operand : TOpCodes) : integer;overload;

     function Count : Integer;


    function AddDEFINE_GLOBAL(const index : integer) : Integer;
    function AddGET_GLOBAL(const Index : integer) : Integer;
    function AddSET_GLOBAL(const Index : integer): Integer;
    function AddConstant(const value : pValue) : integer;
    function EmitConstant(const value : pValue) : integer;
    function AddReturn : integer;
    Function AddNIL : Integer;
    Function AddTRUE : Integer;
    Function AddFALSE : Integer;
    Function AddPOP : Integer;
    Function AddSET_LOCAL : Integer;
    Function AddGET_UPVALUE : Integer;
    Function AddSET_UPVALUE : Integer;
    Function AddGET_PROPERTY : Integer;
    Function AddSET_PROPERTY : Integer;
    Function AddGET_SUPER : Integer;
    Function AddEQUAL : Integer;
    function AddNotEQUAL: Integer;
    Function AddGREATER : Integer;
    Function AddLESS : Integer;
    Function AddLESSTHANEQUAL : integer;
    function AddGREATERTHANEQUAL: Integer;
    Function AddADD : Integer;
    Function AddSUBTRACT : Integer;
    Function AddMULTIPLY : Integer;
    Function AddDIVIDE : Integer;
    Function AddNOT : Integer;
    Function AddNEGATE : Integer;
    Function AddPRINT : Integer;
    Function AddJUMP : Integer;
    Function AddJUMP_IF_FALSE : Integer;
    Function AddLOOP : Integer;
    Function AddINVOKE : Integer;
    Function AddSUPER_INVOKE : Integer;

    Function AddINHERIT : Integer;
    Function AddMETHOD  : Integer;
    constructor Create(const FunctionName : String);
    destructor destroy;override;
    function BytesToString : TStrings;

    property Code[const index : integer] : integer read getCode write setCode; default;
    property Constant[const index : integer] : pValue read getConstant write setConstant;
    property ConstantCount : integer read getConstantCount;
    property OwnsValues : boolean read getOwnsValues;
    //property Constants : TConstants read FConstants;
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


  TValue = record
  private
    function getNumber  : TNumber;
    procedure SetNumber(const value : TNumber);
    function BoolToString : String;
    function getBoolean : Boolean;
    procedure setBoolean(const value : Boolean);
    procedure setObject(const value : pLoxObject);
    function  getObject : pLoxObject;
    function  getFunction : pLoxFunction;
    procedure setNull(const value : Boolean);
    function getNull: boolean;
    procedure SetString(Const value : String);
    function GetString : String;
    function getIsNumber: Boolean;
    function getIsStringObject: Boolean;
    function getIsObject : Boolean;         
    function getIsFunction : Boolean;
    function getIsNull: Boolean;
    function getIsNative: Boolean;
    function getNative: pLoxNative;
    procedure setNative(const Value: pLoxNative);

    function getKind: TLoxKind;
    procedure setKind(const Value: TLoxKind);
    function getIsList: Boolean;
    function getList: pLoxList;
    function getIsBoolean: Boolean;
    function getLoxString: pLoxString;

  public
    ValueRecord : TValueRecord;
    property IsFunction     : Boolean read getIsFunction;
    property IsObject       : Boolean read getisobject;
    property IsString       : Boolean read getIsStringObject;
    property IsList         : Boolean read getIsList;
    property IsNative       : Boolean read getIsNative;
    property IsNumber       : Boolean read getIsNumber;
    property IsNull         : Boolean read getIsNull;
    property isBoolean      : Boolean read getIsBoolean;
    property Kind           : TLoxKind read getKind;
    property Number         : TNumber read getNumber write SetNumber;
    property Boolean        : Boolean read getBoolean write setBoolean;
    property Str            : String read getString write setString;
    property List           : pLoxList read getList;
    property LoxString      : pLoxString read getLoxString;
    property Loxfunction    : pLoxFunction read getFunction;
    property NativeFunction : pLoxNative read getNative write setNative;
    property ToString       : string read getString;
    property Null           : boolean read getNull write setNull;
  end;

  TValueList = class
  private
    FItems      : TList;
    FOwnValues  : Boolean;
  protected
    function  GetItem(const index : integer) : pValue;
    procedure SetItem(const index : integer; const value : pValue);

  public
    function indexof(const value : pValue) : integer;
    function Remove(const index : integer): pValue;
    function Count : integer;
    function Add(const value : pValue) : integer;
    constructor create(Const OwnValues : Boolean);
    destructor destroy;override;
    property OwnsValues : boolean read FOwnValues;
    property Item[const Index : integer] : pValue read getItem write setItem;default;
 end;



  TLoxList = record
     LoxObject : TLoxObject;
     Name      : string;
     Items     : TValueList;
  end;

  //note : Stack top always points to the next place the value will go.
  // [a,b,c,d, sp]  where sp is equal to next avail slot.
  //it's basically the count, noting that the array starts at index 0
  TStack = class
  const
    Stack_Mulitplier = 2;
  private
    FCapacity : integer;
    FItems : Array of pValue;
    FStackTop : integer;
    function getStackTop: integer;
    procedure setStackTop(const Value: integer);
    function getCapacity: integer;
    function getItem(const index: integer): pValue;
    procedure setItem(const index: integer; const Value: pValue);
  protected
    procedure IncreaseCapacity;
  public
    function peek : pValue; overload;
    function Peek(const fromTop : integer) : pValue; overload;
    procedure Push(const value : pValue);
    function pop : pValue;

    constructor create;
    destructor destroy;override;
    property StackTop : integer read getStackTop write setStackTop;
    property Capacity : integer read getCapacity;
    property Item[const index : integer] : pValue read getItem write setItem; default;
  end;


(* TValueStack = class
 private
    FItems  : TValueList;
    function getCount: integer;
 protected
    function GetItem(const index : integer) : pValue;
    procedure setItem(const index : integer; const value : pValue);
    procedure SetStackTop(const value: integer);
 public
    procedure Pop(const amount : integer); overload;
    function Peek : pValue; overload;
    function Peek(const distance : integer) : pValue; overload;
    procedure Push(const Item : pValue);
    function  Pop : pValue; overload;
    constructor create;
    destructor destroy;override;
    property Item[const index : integer] : pValue read getItem write setItem; default;
    property Count : integer read getCount;
    property StackTop : integer write setStackTop;
  end;  *)

  TInstructionPointer = class
  private
    FName     : String;
    FFunction : PLoxFunction;
    FIndex    : integer;
    function getCount: integer;
    function Getconstant(const Index : integer) : pValue;
    function Getglobal(const index : integer) : pValue;
    function getName: String;
    function getFunction: PLoxFunction;
    procedure setFunction(const Value: PLoxFunction);
  public
    function Move(const index : integer) : boolean;
    function OpCodeCount : integer;
    function GetValue(const Index : integer) : Integer;
    function Index : integer;
    function Current : Integer;
    function Next : Integer;
    function PeekNext : Integer;
    constructor create;
    //constructor create(
    //    const loxFunction : PLoxFunction);
    property count : integer read getCount;

    property constant[const index : integer] : pValue read getConstant;
    property global[const index : integer] : pValue read getGlobal;
    property code[const index : integer] :  integer read GetValue; Default;
    property Name : String read getName;
    property Func : PLoxFunction read getFunction write setFunction;
  end;

 TCallFrames = class;

 TCallFrame = class
 private
   FStartCount         : integer;
   FStackTop           : integer;
   FObjectFunction     : pLoxFunction;
   FInstructionPointer : TInstructionPointer;
   FFrameIndex         : integer;
   FStack              : TStack;
   procedure InitInstructionPointer;
   function  getValue(const index: integer): pValue;
   procedure setValue(const index: integer; const Value: pValue);
//    function getStackCount: integer;
 public
    constructor create(
      const ObjectFunction : pLoxFunction;
      const StackTop : integer;
      const Stack : TStack);
   destructor destroy;override;
   property StartCount : integer read fStartCount;
//   property StackCount : integer read getStackCount;
   property LoxFunction : pLoxFunction read FObjectFunction;
   property InstructionPointer : TInstructionPointer read FInstructionPointer;
   property FrameIndex : integer read FFrameIndex;
   property Value[const index : integer] : pValue read getValue write setValue;default;
   property StackTop : integer read FStackTop write FStackTop;

 end;

  TCallFrames = class
  private
    FConstants : TValueList;

    FFrames : TList;
    FFrame  : TCallFrame;

  protected
     function GetFrame : TCallFrame;
     function getCount : integer;

  public
    constructor create;
    destructor destroy; override;
    function Add(
      const StackTop : integer;
      const Stack : TStack;
      const ObjectFunction : pLoxFunction) : TCallFrame;
    function Remove(const Frame : TCallFrame) : integer;

    property Frame : TCallFrame read getFrame;
    property Count : integer read getCount;

  end;

implementation

uses
  sysutils,
  dialogs,
  Exceptions,
  ValueManager;

procedure TValue.setNative(const Value: pLoxNative);
begin
  ValueRecord.Kind := lxNative;
  ValueRecord.Obj := Value;
end;

procedure TValue.setNull(const value : Boolean);
begin
  ValueRecord.Kind := lxNull;
end;

procedure TValue.setObject(const value : pLoxObject);
begin
  ValueRecord.Kind := lxObject;
end;

procedure TValue.SetString(const value: String);
var
  Obj : pLoxObject;
begin
  Assert(getIsStringObject = true, 'Value is not a string object');
  Obj := GetObject;
  if Obj <> nil then
    pLoxString(Obj).Chars := Value;
end;

function TValue.getObject : pLoxObject;
begin
  result := nil;
  if (ValueRecord.Kind = lxObject) or
     (ValueRecord.Kind = lxString) or
     (valuerecord.Kind = lxNative) or
     (valueRecord.Kind = lxfunction) or
     (valueRecord.Kind = lxList) then
    result := ValueRecord.Obj;
end;


function TValue.BoolToString : String;
begin
  if getBoolean = true then result := 'True'
  else
    result := 'False';

end;

function TValue.GetString: String;
var
  Obj : pLoxObject;
  fun : pLoxFunction;
begin
  case  ValueRecord.Kind of
   lxObject : begin
       Obj := GetObject;
       case Obj.Kind of
         OBJ_STRING : begin
           result := pLoxString(obj).Chars;
         end;
       end;
   end;

   lxFunction : begin
       fun := getFunction;
       result := fun^.name;

   end;

   lxBoolean : begin
     result := BoolToString;
   end;

   lxNumber : begin
     result := floatToStr(GetNumber);
   end;

   lxNull  : begin
     result := 'null';
   end;

   lxString : begin
     result := pLoxString(ValueRecord.Obj).Chars;
   end;

  end;
end;

function TValue.getBoolean : Boolean;
begin
  result := false;
  if  ValueRecord.Kind = lxBoolean then result := ValueRecord.Bool;

end;

function TValue.getFunction: pLoxFunction;
begin
  result := nil;
  if  ValueRecord.Kind = lxFunction then
     result := pLoxFunction(ValueRecord.Obj);
end;

procedure TValue.setBoolean(const value : Boolean);
begin
  assert(IsBoolean, 'value is not a boolean so can''t set it to one');
  ValueRecord.Kind := lxBoolean;
  ValueRecord.Bool := Value;
end;

procedure TValue.setKind(const Value: TLoxKind);
begin
  ValueRecord.Kind := Value;
end;

function TValue.getIsBoolean: Boolean;
begin
   result := ValueRecord.Kind = lxBoolean;
end;

function TValue.getIsFunction: Boolean;
begin
   result := ValueRecord.Kind = lxFunction;
end;

function TValue.getIsList: Boolean;
begin
  result := ValueRecord.Kind = lxList;
end;

function TValue.getIsNative: Boolean;
begin
  result := ValueRecord.Kind = lxNative;
end;

function TValue.getIsNull: Boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function TValue.getIsNumber: Boolean;
begin
  result := ValueRecord.Kind = lxNumber;
end;

function TValue.getIsObject: Boolean;
begin
   result := (ValueRecord.Kind = lxObject)
end;

function TValue.getIsStringObject: Boolean;
begin
  result :=  ValueRecord.Kind = lxString;//getisobject and (getObject.Kind = OBJ_STRING);
end;

function TValue.getKind: TLoxKind;
begin
  result := ValueRecord.Kind
end;

function TValue.getList: pLoxList;
begin
  result := nil;
  if valueRecord.Kind = lxList then
    result := pLoxList(valueRecord.obj);
end;

function TValue.getLoxString: pLoxString;
begin
  result := nil;
  if ValueRecord.Kind = lxString then
    result := pLoxString(ValueRecord.obj);
end;

function TValue.getNative: pLoxNative;
begin
  result := nil;
  if ValueRecord.Kind = lxNative then
     result := pLoxNative(ValueRecord.Obj);
end;

Function TValue.getNull: boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function TValue.getNumber : TNumber;
begin
  result := -1;
  if ValueRecord.Kind = lxNumber then
  begin
    result := ValueRecord.Number;
  end;
end;

procedure TValue.SetNumber(const value : TNumber);
begin
   assert(IsNumber, 'value is not a number so can''t set it to one');
   ValueRecord.Kind := lxNumber;
   ValueRecord.Number := Value;

end;


(*
constructor TValueStack.create;
begin
  FItems := TValueList.Create(false);
end;

destructor TValueStack.Destroy;
begin
  FItems.Free;
  inherited;
end;




function TValueStack.getCount: integer;
begin
  result := FItems.Count;
end;

function TValueStack.GetItem(const index: integer): pValue;
begin
  result := FItems.item[index];
end;


//distance from the count basically
function TValueStack.Peek(const distance: integer): pValue;
begin
  assert(distance >= 0, 'distance is negative - this is from the top');
  assert(FItems.Count - 1 - distance >=0, 'distance outside bounds');
  result := FItems.Item[FItems.Count -1 - distance];

end;

procedure TValueStack.Pop(const amount: integer);
var
  i : integer;
  value : pValue;
begin
  for i := 0 to amount-1 do
  begin
    Value := Pop;

  end
end;

function TValueStack.Peek: pValue;
begin
  result := Peek(0);
end;

function TValueStack.Pop: pValue;
var
  removed : pValue;
begin
  assert(FItems.Count > 0, 'No items on stack');

  result := FItems.Remove(FItems.Count-1);

end;

procedure TValueStack.Push(const Item: pValue);
begin
  FItems.Add(Item);
end;

//1,2,3,4,5,6,7,8,9,10,11,12   = stacktop = 13

//get rid of items beyond the current stack top
procedure TValueStack.SetStackTop(const value: integer);
var
  i : integer;
begin
  assert(Value <= FItems.count, 'moving the stack right');
  for i := FItems.count-1 downto Value  do
  begin
    pop;
  end
end;

procedure TValueStack.setItem(const index: integer; const value: pValue);
begin
  FItems[index] := value;
end;

 *)



{ TValueList }

function TValueList.Add(const value: pValue): integer;
begin
  assert(assigned(Value), 'Value being inserted is nil');
  result := FItems.add(value);
end;

function TValueList.Remove(const index : integer) : pValue;
begin
   result := FItems[index];
   FItems.Delete(index);
end;


function TValueList.Count: integer;
begin
  result := FItems.Count;
end;

constructor TValueList.create(const OwnValues: Boolean);
begin
  FOwnValues := OwnValues;
  FItems := TList.create;
end;


destructor TValueList.destroy;
var
  i : integer;
  p : pValue;
begin
  p := nil;
  if FOwnValues then
  begin
    for i := 0 to FItems.Count-1 do
    begin
      p := Item[i];
      assert(p <> nil, 'finalize value item expected non nil value');
      BorrowChecker.Dispose(p);
      p := nil;
    end;
  end;

  FItems.Free;
  inherited;
end;

function TValueList.GetItem(const index: integer): pValue;
begin
  result := FItems[index];
end;


function TValueList.indexof(const value: pValue): integer;
begin
  result := FItems.IndexOf(Value);
end;

procedure TValueList.SetItem(const index: integer; const value: pValue);
begin
  //if FItems[Index] <> nil then raise exception.create('value already assigned at index');
  FItems[index] := value;
end;



function TInstructionPointer.GetCount: integer;
begin
  result := FFunction.Chunks.Count;
end;

function TInstructionPointer.getFunction: PLoxFunction;
begin
  result := FFunction;
end;

function TInstructionPointer.Getglobal(const index: integer): pValue;
begin
   result := FFunction.Chunks.Constant[Index];
end;



function TInstructionPointer.getName: String;
begin
  result := '';
  if not assigned(FFunction) then exit;
  result := FFunction.Name;
end;


function TInstructionPointer.GetValue(const Index : integer) : integer;
begin
  assert((index >= 0) and (index < FFunction.Chunks.count));
  result := FFunction.Chunks[FIndex];
end;

constructor TInstructionPointer.create;
begin
  fIndex := -1;
end;

function TInstructionPointer.Current: integer;
begin
  result := getValue(FIndex);
end;

function TInstructionPointer.Getconstant(const Index: integer): pValue;
begin
  result := FFunction.Chunks.Constant[Index];
end;

function TInstructionPointer.Index: integer;
begin
  result := FIndex;
end;

(*
constructor TInstructionPointer.create(
  const loxFunction: PLoxFunction);

begin
  FFunction := LoxFunction;
  FIndex := -1;
end;
*)

function TInstructionPointer.Move(const index: integer): boolean;
begin
  result := false;
  if (index >= 0) and (index < FFunction.Chunks.Count) then
  begin
    FIndex := index;
    result := true;
  end;
end;


function TInstructionPointer.Next: integer;
begin
  result := -1;
  if FFunction.Chunks.Count = 0 then exit;

  inc(FIndex);
  if FIndex = FFunction.Chunks.Count then exit;
  result := Current;
end;

function TInstructionPointer.OpCodeCount: integer;
begin

end;

function TInstructionPointer.PeekNext: Integer;
var
  i : integer;
begin
  result := -1;
  if FFunction.Chunks.Count = 0 then exit;
  i := FIndex;
  inc(i);
  if i = FFunction.Chunks.Count then exit;
  result :=  FFunction.Chunks[i];
end;


procedure TInstructionPointer.setFunction(const Value: PLoxFunction);
begin
  FFunction := Value;
end;

function TCallFrames.Add(
  const StackTop : integer;
  const Stack : TStack;
  const ObjectFunction: pLoxFunction): TCallFrame;
begin
  try
    result := TCallFrame.Create(ObjectFunction,StackTop,Stack);
  except
    on E:Exception do
    begin
      Showmessage('Failed to create call frame @ index : ' + inttostr(FFrames.Count));
      exit;
    end;
  end;
  FFrames.Add(result);
end;

constructor TCallFrames.create;
begin
  FFrames := Tlist.create;

end;

destructor TCallFrames.destroy;
var
  i : integer;
begin
  for i := FFrames.Count-1 downto 0 do
  begin
    TCallFrame(FFrames[i]).free;
  end;
  FFrames.Free;
  
end;

function TCallFrames.getCount: integer;
begin
  result := FFrames.Count;
end;

function TCallFrames.GetFrame: TCallFrame;
begin
  result := nil;
  if FFrames.Count > 0 then
    result := FFrames[FFrames.Count-1];
end;



function TCallFrames.Remove(const Frame: TCallFrame): integer;
begin
  result := FFrames.Remove(Frame);
end;

{ TCallFrame }


procedure TCallFrame.InitInstructionPointer;
begin
  FInstructionPointer := TInstructionPointer.Create;
  FInstructionPointer.Func := FObjectFunction;
end;

constructor TCallFrame.create(
  const ObjectFunction : pLoxFunction;
  const StackTop : integer;
  const Stack : TStack);
begin
   FObjectFunction := ObjectFunction;
   InitInstructionPointer;
   FStack := Stack;
   FStackTop := StackTop;
end;

destructor TCallFrame.destroy;
begin
  FInstructionPointer.Free;
  inherited;
end;

(*function TCallFrame.getStackCount: integer;
begin
  result := FStack.Count;
end; *)
 
function TCallFrame.getValue(const index: integer): pValue;
begin
  result := FStack[StackTop + index];
end;

procedure TCallFrame.setValue(const index: integer; const Value: pValue);
begin
   FStack[StackTop + index] := Value;
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



function NewOpCode(const OPCode : TOpCodes) : pByte;
begin
  new(result);
  result^ := byte(opCode);
end;

function newByte(const value : byte) : pByte;
begin
  new(result);
  result^ := value;
end;

 function TChunks.Emit(const Operand : TOpCodes) : integer;
 begin
   result := FCode.Add(ord(Operand));
 end;

function TChunks.Emit(const value : integer) : integer;
begin
  result := FCode.Add(value);
end;


Function TChunks.Emit(const Operand : TOpCodes; const  value : integer) : integer;
begin
  result := emit(Operand);
  emit(Value);
end;

Function TChunks.Emit(const valueA, ValueB : Integer) : integer;
begin
  result := emit(ValueA);
  emit(ValueB);
end;



function TChunks.getCode(const index: integer): integer;
begin
  result := FCode[index];
end;

function TChunks.getConstant(const index: integer): pValue;
begin
  result := FConstants[Index];
end;

function TChunks.getConstantCount: integer;
begin
  result := FConstants.Count;
end;

function TChunks.getOwnsValues: boolean;
begin
  result := FConstants.OwnValues;
end;

procedure TChunks.setCode(const index, Value: integer);
begin
  FCode[Index] := Value;
end;

procedure TChunks.setConstant(const index : integer; const Value: pValue);
begin
  FConstants[Index] := Value;
end;

function TChunks.AddConstant(const value: pValue): integer;
begin
  result := FConstants.Add(Value);
end;

function TChunks.AddDEFINE_GLOBAL(const index : integer) : Integer; //index of the global constant
begin
  result := Emit(OP_DEFINE_GLOBAL,index);
end;

function TChunks.AddGET_GLOBAL(const Index : integer) : Integer;
begin
  result := Emit(OP_GET_GLOBAL, index);
end;

function TChunks.AddSET_GLOBAL(const Index : integer): Integer;
begin
  result := Emit(OP_SET_GLOBAL,index);
end;

function TChunks.AddDIVIDE: Integer;
begin
  result := Emit(OP_DIVIDE);
end;

function TChunks.AddEQUAL: Integer;
begin
  result := Emit(OP_EQUAL);
end;

function TChunks.AddNotEQUAL: Integer;
begin
  result := AddEqual;
  AddNOT;
end;

function TChunks.AddFALSE: Integer;
begin
  result := Emit(OP_FALSE);
end;


function TChunks.AddGET_PROPERTY: Integer;
begin
  result := Emit(OP_GET_PROPERTY);
end;

function TChunks.AddGET_SUPER: Integer;
begin
  result := Emit(Integer(OP_GET_SUPER));
end;

function TChunks.AddGET_UPVALUE: Integer;
begin
  result := Emit( OP_GET_UPVALUE);
end;

function TChunks.AddGREATER: Integer;
begin
  result := Emit(OP_GREATER);
end;

function TChunks.AddGREATERTHANEQUAL: Integer;
begin
  result := AddLESS;
  result := AddNOT;
end;

function TChunks.AddINHERIT: Integer;
begin
  result := Emit( OP_INHERIT);
end;

function TChunks.AddINVOKE: Integer;
begin
  result := Emit(OP_INVOKE);
end;

function TChunks.AddJUMP: Integer;
begin
 result := Emit(OP_JUMP);
end;

function TChunks.AddJUMP_IF_FALSE: Integer;
begin
  result := Emit(OP_JUMP_IF_FALSE);
end;

function TChunks.AddLESS: Integer;
begin
  result := Emit(OP_LESS);
end;

function TChunks.AddLESSTHANEQUAL: integer;
begin
  result := AddGreater; 
  result := AddNot;
end;

function TChunks.AddLOOP: Integer;
begin
  result := Emit(OP_LOOP);
end;

function TChunks.AddMETHOD: Integer;
begin
  result := Emit(OP_METHOD);
end;

function TChunks.AddMULTIPLY: Integer;
begin
  result := Emit(OP_MULTIPLY);
end;

function TChunks.AddNEGATE: Integer;
begin
  result := Emit(OP_NEGATE);
end;

function TChunks.AddNIL: Integer;
begin
  result := Emit(OP_NIL);
end;

function TChunks.AddNOT: Integer;
begin
  result := Emit(OP_NOT);
end;

function TChunks.AddPOP: Integer;
begin
  result := Emit(OP_POP);
end;

function TChunks.AddPRINT: Integer;
begin
  result := Emit(OP_PRINT);
end;

function TChunks.AddReturn : integer;
begin
  result := Emit(OP_RETURN);
end;

function TChunks.AddSET_LOCAL: Integer;
begin
  result := Emit(OP_SET_LOCAL);
end;

function TChunks.AddSET_PROPERTY: Integer;
begin
   result := Emit(OP_SET_PROPERTY);
end;

function TChunks.AddSET_UPVALUE: Integer;
begin

  result := Emit(OP_SET_UPVALUE);
end;


function TChunks.AddADD: Integer;
begin

  result := Emit(OP_ADD);
end;

function TChunks.AddSUBTRACT: Integer;
begin

  result := Emit(OP_SUBTRACT);
end;

function TChunks.AddSUPER_INVOKE: Integer;
begin

  result := Emit(OP_SUPER_INVOKE);
end;

function TChunks.AddTRUE: Integer;
begin

  result := Emit(OP_TRUE);
end;

function TChunks.BytesToString: TStrings;
var
  i : integer;
begin
  result := TStringList.create;

  for i := 0 to FCode.Count-1 do
  begin
    result.add(Inttostr(FCode[i]));
  end;
end;


function TChunks.EmitConstant(const value : pValue) : integer;
begin
  result := FConstants.Add(value);
  Emit(OP_CONSTANT,result);
end;




function TChunks.Count: Integer;
begin
  result := FCode.Count;
end;

constructor TChunks.Create(const FunctionName : String);
begin
  FName := Functionname;
  FCode := TOpCode.Create;
  FConstants := TConstants.Create(Fname);

end;

destructor TChunks.destroy;
begin
  FCode.Free;
  FConstants.Free;
  inherited;
end;

{ TConstants }              
constructor TConstants.create(const functionName : String);
begin
  FName := FunctionName;
  FItems := TValueList.create(false);
end;


destructor TConstants.destroy;
begin
  FItems.Free;
  inherited;
end;

function TConstants.Add(const value: pValue): integer;
begin
  result := FItems.Add(Value);
end;

function TConstants.getCount: integer;
begin
  result := FItems.Count;
end;

function TConstants.getItem(const Index: integer): pValue;
begin
  result := FItems[index];
end;

function TConstants.getOwnValues: boolean;
begin
  result := FItems.OwnsValues;
end;

procedure TConstants.setItem(const Index: integer; const Value: pValue);
begin
  FItems[index] := Value;
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

constructor TStack.create;
begin
  FStackTop := 0; //noting that this is different from the count, since it begins at slot 0
  FCapacity := Stack_Mulitplier;
  setLength(FItems,FCapacity);
end;

destructor TStack.destroy;
begin

  inherited;
end;

function TStack.getCapacity: integer;
begin
  result := FCapacity;
end;

function TStack.getStackTop: integer;
begin
  result := FStackTop;
end;

function TStack.getItem(const index: integer): pValue;
begin
  assert(index >= 0, 'index is negative to stack bottom');
  assert(index <= FStackTop, 'index is beyond stack top');
  result := FItems[index];
end;

procedure TStack.IncreaseCapacity;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems,FCapacity);
end;

function TStack.peek: pValue;
begin
  result := FItems[FStackTop-1];
end;

function TStack.peek(const fromTop: integer): pValue;
begin
  assert(FromTop >= 0, 'This is distance from the top as a positive');
  assert(FStackTop - FromTop >= 0, 'Distance is beyond stack bottom');
  result := FItems[FStackTop-FromTop-1];
end;

function TStack.pop: pValue;
begin
  assert(FStackTop >= 1, 'No items to pop');
  result := FItems[FStackTop-1];
  FStackTop := FStackTop - 1;
end;

procedure TStack.Push(const value: pValue);
begin
  FItems[FStackTop] := value;
  SetStackTop(FStackTop+1);
end;

procedure TStack.setItem(const index: integer; const Value: pValue);
begin
  assert(index >= 0, 'index is negative to stack bottom');
  assert(index <= FStackTop, 'index is beyond stack top');
  FItems[index] := Value;
end;

procedure TStack.setStackTop(const Value: integer);
begin
  if Value > FCapacity-1 then   //since the FStackTop starts at 0
  begin
    IncreaseCapacity;
  end;
  FStackTop := Value;
end;

end.