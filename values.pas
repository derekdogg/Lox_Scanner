unit Values;

interface

uses classes, LoxTypes, OpCodes;


type

  pValue = ^TValue;

  TValueStack = class;

  TNativeFunction = function(const ArgCount: Integer; const Values : TValueStack): pValue;

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

  TLoxKind = (lxNumber,lxBoolean, lxNull, lxObject, lxFunction, lxNative);

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
  protected

  public
    constructor create(const functionName : String); //to help with debugging stuff
    destructor destroy; override;
    function Add(const value : pValue) : integer;
    property Item[const Index : integer] : pValue read getItem write setItem;default;
    property Count : integer read getCount;
  end;




  TChunks = class
  private
    //FConstantCount : integer;
    FName : String;
    FOnChunk   : TOnChunk;
    FBytes     : TList;
    FConstants : TConstants;
    function getValue(const index: integer): pByte;



    // FLine      : TIntegers;
  public
    procedure DoChunk(const Operand : TOpCodes);

    function Count : Integer;

    Function EmitBytes(const Operand : TOpCodes; const  byte2 : byte) : integer;overload;
    function EmitBytes(const byte1, byte2 : byte) : Integer;overload;
    function EmitByte(const value : byte) : integer; overload;
    function EmitByte(const Operand : TOpCodes) : integer;overload;

    function AddDEFINE_GLOBAL(const index : byte) : Integer;
    function AddGET_GLOBAL(const Index : integer) : Integer;
    function AddSET_GLOBAL(const Index : integer): Integer;

    procedure EmitConstant(const value : pValue);

//    function EmitConstant(const Value: pointer) : Integer; //rename to check all instances
    function AddReturn : integer;
    Function AddNIL : Integer;
    Function AddTRUE : Integer;
    Function AddFALSE : Integer;
    Function AddPOP : Integer;
   // Function AddGET_LOCAL : Integer;
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
    //Function AddCALL : Integer;
    Function AddINVOKE : Integer;
    Function AddSUPER_INVOKE : Integer;
//    Function AddCLOSURE : Integer;
//    Function AddCLOSE_UPVALUE : Integer;
//    Function AddCLASS : Integer;
    Function AddINHERIT : Integer;
    Function AddMETHOD  : Integer;
    constructor Create(const FunctionName : String);
    destructor destroy;override;
    function BytesToString : TStrings;

    property Item[const index : integer] : pByte read getValue; default;
    property Constants : TConstants read FConstants;
  end;

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
    LoxObject : TLoxObject;
    FuncKind  : TFunctionKind;
    Arity     : integer; // The arity field stores the number of parameters the function expects.
    Name      : String;
    Chunks    : TChunks;
  end;

  TValue = record
  private

    FKind  : TLoxKind;
    FValue : TEightBytes;
    function getNumber  : TNumber;
    procedure SetNumber(const value : TNumber);
    function BoolToString : String;
    function getBoolean : Boolean;
    procedure setBoolean(const value : Boolean);
    procedure setObject(const value : pLoxObject);
    function  getObject : pLoxObject;
    procedure setFunction(const value : pLoxFunction);
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
  public
    property IsFunction     : Boolean read getIsFunction;
    property IsObject       : Boolean read getisobject;
    property IsString       : Boolean read getIsStringObject;
    property IsNative       : Boolean read getIsNative;
    property IsNumber       : Boolean read getIsNumber;
    property IsNull         : Boolean read getIsNull;
    property Kind           : TLoxKind read FKind write FKind;
    property Number         : TNumber read getNumber write SetNumber;
    property Boolean        : Boolean read getBoolean write setBoolean;
    property Str            : String read getString write setString;
    property Bytes          : TEightBytes read FValue;
    property LoxObject      : pLoxObject read getObject write setObject;
    property Loxfunction    : pLoxFunction read getFunction write SetFunction;
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
    property Item[const Index : integer] : pValue read getItem write setItem;default;
 end;



 TValueStack = class
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
 end;







  TInstructionPointer = class
  private
    FName     : String;
    FFunction : PLoxFunction;
    FConstants  : TConstants;

    FIndex      : integer;
    FCurrent    : pByte;
    FPrevious   : pByte;
    function getCount: integer;
    function Getconstant(const Index : integer) : pValue;
    function Getglobal(const index : integer) : pValue;
    function getName: String;
    function getFunction: PLoxFunction;


  public
    function Move(const index : integer) : boolean;
    function OpCodeCount : integer;
    function ConstantCount : integer;
    function ByteAt(const Index : integer) : pByte;
    function Index : integer;
    function CurrentByte : pByte;
    function Next : pByte;
    function PeekNext : pByte;
    constructor create(
        const loxFunction : PLoxFunction);
    property count : integer read getCount;

    property constant[const index : integer] : pValue read getConstant;
    property global[const index : integer] : pValue read getGlobal;
    property code[const index : integer] :  pByte read ByteAt;Default;
    property Name : String read getName;
    property Func : PLoxFunction read getFunction;
  end;

    //so the value (I think) is an index into an array of pValue. If you have 1, you can either increment it, or decrement it.

 TCallFrames = class;

 TCallFrame = class
 private
   FStartCount         : integer;

   FStackTop           : integer;

   FObjectFunction     : pLoxFunction;

   FInstructionPointer : TInstructionPointer;

   FFrameIndex         : integer;

   FStack              : TValueStack;

   function  getValue(const index: integer): pValue;
   procedure setValue(const index: integer; const Value: pValue);
    function getStackCount: integer;
 public
   property StartCount : integer read fStartCount;
   property StackCount : integer read getStackCount;
   property LoxFunction : pLoxFunction read FObjectFunction;
   property InstructionPointer : TInstructionPointer read FInstructionPointer;
//   property Value : TValueList read FValues write FValues;
   property FrameIndex : integer read FFrameIndex;

   property Value[const index : integer] : pValue read getValue write setValue;default;
   //property Stack : TValueStack read FValueStack write FValueStack;
   constructor create(
      const ObjectFunction : pLoxFunction;
      const StackTop : integer;
      const Stack : TValueStack);
   destructor destroy;override;

   property StackTop : integer read FStackTop write FStackTop;

 end;

  TCallFrames = class
  private

    FConstants : TValueList;

    FValueStack : TValueStack;
    FFrames : TList;
    FFrame  : TCallFrame;

  protected
     function GetFrame : TCallFrame;
     function getCount : integer;
     function getValueStack : TValueStack;
  public
    constructor create;
    destructor destroy; override;
    function Add(
      const StackTop : integer;
      const Stack : TValueStack;
      const ObjectFunction : pLoxFunction) : TCallFrame;
    function Remove(const Frame : TCallFrame) : integer;

    property Frame : TCallFrame read getFrame;
    property Count : integer read getCount;
    property Stack : TValueStack read getValueStack;

  end;

  function newNative(NativeFn : TNativeFunction) : pValue;
  function newValueFromFunction(const fn : pLoxFunction) : pValue;
  function NewFunction(const name : string; const Kind :  TFunctionKind) : pValue;
  function NewString(const str : String) : pValue;
  function NewNumber(Const number : TNumber) : pValue;
  function NewBool(const Bool : Boolean) : pValue;
  function DisposeValue(value : pValue) : boolean;
  function newLoxFunction(const Name : String) : pLoxFunction;
     procedure disposeFunction(LoxFunction : pLoxFunction);


implementation
uses sysutils,
     Exceptions;


function newLoxFunction(const Name : String) : pLoxFunction;
begin
    new(result);
    result.LoxObject.Kind := OBJ_FUNCTION;
    result.LoxObject.Next := nil;
    result.FuncKind := Type_Function;
    result.Arity := 0;
    result.Name := Name;
    result.Chunks := TChunks.Create(Name);//(Constants); //.Init;
end;


procedure disposeFunction(LoxFunction : pLoxFunction);
begin
  LoxFunction.Chunks.Free;
  dispose(LoxFunction);
end;




function newValueFromFunction(const fn : pLoxFunction) : pValue;
begin
  new(result);
  result.LoxFunction := fn;
  result.Kind := lxFunction;
end;

function NewFunction(const name : string; const Kind :  TFunctionKind) : pValue;
var
  fn : pLoxFunction;
begin
  new(result);
  new(fn);
  fn.Name := name;
  fn.Chunks := TChunks.Create(name);
  result.LoxFunction := fn;
  result.Kind := lxFunction;

end;


function newNative(NativeFn : TNativeFunction) : pValue;
var
  LoxNative : PLoxNative;
begin
  new(result);
  new(loxNative);
  loxNative.Native := NativeFn;

  result.NativeFunction := LoxNative;
end;


function NewString(const str : String) : pValue;
begin
  new(result);
  result.LoxObject :=  pLoxObject(NewLoxString(str));//( LoxObjectFrom(NewLoxString(str));
end;

function NewBool(const Bool : Boolean) : pValue;
begin
  new(result);
  result.Boolean := Bool;
end;

function NewNumber(const number : TNumber) : pValue;
begin
  new(result);
  result.Number := Number;
end;

function DisposeValue(value : pValue) : boolean;
var
  loxString   : pLoxString;
  loxFunction : pLoxFunction;
  loxNative   : pLoxNative;
begin
  assert(assigned(Value), ' value for disposal is nil');
  if value.IsString then
  begin
    assert(assigned(Value.LoxObject), 'Lox Object for disposal is not assigned');
    LoxString := pLoxString(Value.LoxObject);
    dispose(LoxString);
  end;

  if value.IsNative then
  begin
    LoxNative := Value.NativeFunction;
    dispose(LoxNative);
  end;


  if value.IsFunction then
  begin
    LoxFunction := pLoxFunction(Value.LoxFunction);
    LoxFunction.Chunks.Free;
    Dispose(LoxFunction);
  end;

  dispose(Value);
  value := nil;
end;



procedure TValue.setNative(const Value: pLoxNative);
begin
  fKind := lxNative;
  Move(Longint(value),FValue[0], SizeOf(Value));
end;

procedure TValue.setNull(const value : Boolean);
begin
  FKind := lxNull;
  fillchar(FValue,sizeof(FValue),#0);
end;

procedure TValue.setObject(const value : pLoxObject);
begin
  FKind := lxObject;
  Move(Longint(value),FValue[0], SizeOf(Value));
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
  if (FKind = lxObject) then
    Move(FValue[0], Result, SizeOf(Result));
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
  case FKind of
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


  end;
end;

function TValue.getBoolean : Boolean;
begin
  result := false;
  if FKind = lxObject then
  begin
    result := getNumber <=0;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(Result))
end;

function TValue.getFunction: pLoxFunction;
begin
  result := nil;
  if FKind = lxFunction then
    Move(FValue[0], Result, SizeOf(Result));
end;

procedure TValue.setBoolean(const value : Boolean);
begin
  assert(FKind <> lxObject,'value is an object, and you are trying to set it to false'); //this is not cool to set the bytes of an existing pointer?
  FKind := lxBoolean;
  FillChar(FValue,Sizeof(FValue),#0);
  Move(value, FValue[0], SizeOf(Value))
end;

procedure TValue.setFunction(const value: pLoxFunction);
begin
  Move(Longint(value),FValue[0], SizeOf(Value));
end;

function TValue.getIsFunction: Boolean;
begin
   result := FKind = lxFunction;
end;

function TValue.getIsNative: Boolean;
begin
  result := fKind = lxNative;
end;

function TValue.getIsNull: Boolean;
begin
  result := FKind = lxNull;
end;

function TValue.getIsNumber: Boolean;
begin
  result := FKind = lxNumber;
end;

function TValue.getIsObject: Boolean;
begin
   result := (fKind = lxObject)
end;

function TValue.getIsStringObject: Boolean;
begin
  result := getisobject and (getObject.Kind = OBJ_STRING);
end;

function TValue.getNative: pLoxNative;
begin
  result := nil;
  if FKind = lxNative then
    Move(FValue[0], Result, SizeOf(Result));
end;

Function TValue.getNull: boolean;
begin
  result := fKind = lxNull;
end;

function TValue.getNumber : TNumber;
var
  l : Longint;
begin
  if FKind = lxObject then
  begin
    Move(FValue[0], l, SizeOf(l));
    result := l;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(TNumber))
end;

procedure TValue.SetNumber(const value : TNumber);
begin
   Fkind := lxNumber;
   Move(value,FValue[0], SizeOf(TNumber))
end;



constructor TValueStack.create;
begin
  FItems := TValueList.Create(true);
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

  if FOwnValues then
  begin
    for i := 0 to FItems.Count-1 do
    begin
      p := Item[i];
      assert(p <> nil, 'finalize value item expected non nil value');
      DisposeValue(p);
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
  assert(index >= 0, 'Index for value is < 0');
  assert(index < FConstants.count, 'index is > than FConstants Count');
  result := FConstants[Index];
end;



function TInstructionPointer.getName: String;
begin
  result := '';
  if not assigned(FFunction) then exit;
  result := FFunction.Name;
end;

function TInstructionPointer.ConstantCount: integer;
begin
  result := FConstants.Count;
end;



function TInstructionPointer.ByteAt(const Index : integer) : pByte;
begin
  assert((index >= 0) and (index < FFunction.Chunks.count));
  result := FFunction.Chunks[FIndex];
end;

function TInstructionPointer.CurrentByte: pByte;
begin
  result := ByteAt(FIndex);//TOpCodes(FBytes.Item(FIndex)^);
end;

function TInstructionPointer.Getconstant(const Index: integer): pValue;
begin

  assert(index >= 0, 'Index for value is < 0');
  assert(index < FConstants.count, 'index is > than FConstants Count');
  result := FConstants[Index];

end;

function TInstructionPointer.Index: integer;
begin
  result := FIndex;
end;

constructor TInstructionPointer.create(
  const loxFunction: PLoxFunction);

begin
  FFunction := LoxFunction;


  FCurrent := nil;
  FPrevious := nil;
//  FBytes := LoxFunction.Chunks.OpCodes;
  FConstants := LoxFunction.Chunks.Constants;
  FIndex := -1;
end;

function TInstructionPointer.Move(const index: integer): boolean;
begin
  result := false;
  if (index >= 0) and (index < FFunction.Chunks.Count) then
  begin
    FIndex := index;
    result := true;
  end;
end;


function TInstructionPointer.Next: pByte;
begin
  result := nil;
  if FFunction.Chunks.Count = 0 then exit;

  inc(FIndex);
  if FIndex = FFunction.Chunks.Count then exit;
  result := CurrentByte;
end;

function TInstructionPointer.OpCodeCount: integer;
begin

end;

function TInstructionPointer.PeekNext: pByte;
var
  i : integer;
begin
  result := nil;
  if FFunction.Chunks.Count = 0 then exit;
  i := FIndex;
  inc(i);
  if i = FFunction.Chunks.Count then exit;
  result :=  FFunction.Chunks[i];
end;


function TCallFrames.Add(
  const StackTop : integer;
  const Stack : TValueStack;
  const ObjectFunction: pLoxFunction): TCallFrame;
begin
  result := TCallFrame.Create(ObjectFunction,StackTop,Stack);

  FFrames.Add(result);
end;

constructor TCallFrames.create;
begin
  FFrames := Tlist.create;
  FValueStack := TValueStack.Create;
end;

destructor TCallFrames.destroy;
begin
  FFrames.Free;

  FValueStack.Free;
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

function TCallFrames.getValueStack: TValueStack;
begin
  result := FValueStack;
end;

function TCallFrames.Remove(const Frame: TCallFrame): integer;
begin
  result := FFrames.Remove(Frame);
end;

{ TCallFrame }

constructor TCallFrame.create(
  const ObjectFunction : pLoxFunction;
  const StackTop : integer;
  const Stack : TValueStack);
begin

   FObjectFunction     := ObjectFunction;
   FInstructionPointer := TInstructionPointer.Create(LoxFunction);


   FStack := Stack;
   FStackTop := StackTop;

end;

destructor TCallFrame.destroy;
begin

  FInstructionPointer.Free;
  inherited;
end;

function TCallFrame.getStackCount: integer;
begin
  result := FStack.Count;
end;

(*    [1][2][3][4][5][6][7][8][9][10][11]  VM Stack, stack top = 8
      [a][b][c][d][e][f][g][ ][ ][ ] [ ]

      Set frame stack top to 4, item zero = 3


*)

function TCallFrame.getValue(const index: integer): pValue;
begin
  result := FStack[index];
end;

procedure TCallFrame.setValue(const index: integer; const Value: pValue);
begin
   FStack[index] := Value;
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

 function TChunks.EmitByte(const Operand : TOpCodes) : integer;
 begin
   DoChunk(Operand);
   EmitByte(Byte(Operand));
 end;

function TChunks.EmitByte(const value : byte) : integer;
begin
  //writeChunk(currentChunk(), byte, parser.previous.line);

  result := FBytes.Add(newByte(value));
end;


Function TChunks.EmitBytes(const Operand : TOpCodes; const  byte2 : byte) : integer;
begin
  result := emitByte(byte(Operand));
  emitByte(byte2);
end;

Function TChunks.EmitBytes(const byte1, byte2 : byte) : integer;
begin
  result := emitByte(byte1);
  emitByte(byte2);
end;



function TChunks.getValue(const index: integer): pByte;
begin
  result := FBytes[index];
end;

(*function TChunks.OpCodes: TList;
begin
  result := FBytes;
end; *)

//add constant opcode followed by index of constant in constants array
(*function TChunks.EmitConstant(const Value: pointer) : Integer;
begin
  (*assert(Assigned(Value), 'Value for emission is nil.. please try again later. Have a nice day you are screwed');
  if FConstants.Count = high(Byte) then raise EMaxConstants.create('Max constants reached'); //note since the opcodes is bytes array, it has fixed index size of 256
  //inc(FConstantCount);
  result := EmitBytes(OP_CONSTANT,MakeConstant(Value));
end;    *)

function TChunks.AddDEFINE_GLOBAL(const index : byte) : Integer; //index of the global constant
begin
  result := EmitBytes(OP_DEFINE_GLOBAL,index);
end;

function TChunks.AddGET_GLOBAL(const Index : integer) : Integer;
begin
  result := EmitBytes(OP_GET_GLOBAL, index);
end;

function TChunks.AddSET_GLOBAL(const Index : integer): Integer;
begin
  result := EmitBytes(OP_SET_GLOBAL,index);
end;

function TChunks.AddDIVIDE: Integer;
begin
  result := EmitByte(OP_DIVIDE);
end;

function TChunks.AddEQUAL: Integer;
begin
  result := EmitByte(OP_EQUAL);
end;

function TChunks.AddNotEQUAL: Integer;
begin
  result := AddEqual;
  AddNOT;
end;

function TChunks.AddFALSE: Integer;
begin
  result := EmitByte(OP_FALSE);
end;



(*function TChunks.AddGET_LOCAL: Integer;
begin
  result := FBytes.Add(ord(OP_GET_LOCAL));
end; *)

function TChunks.AddGET_PROPERTY: Integer;
begin
  result := EmitByte(OP_GET_PROPERTY);
end;

function TChunks.AddGET_SUPER: Integer;
begin
  result := EmitByte(OP_GET_SUPER);
end;

function TChunks.AddGET_UPVALUE: Integer;
begin
  result := EmitByte(OP_GET_UPVALUE);
end;

function TChunks.AddGREATER: Integer;
begin
  result := EmitByte(OP_GREATER);
end;

function TChunks.AddGREATERTHANEQUAL: Integer;
begin
  result := AddLESS;
  result := AddNOT;
end;

function TChunks.AddINHERIT: Integer;
begin
  result := EmitByte(OP_INHERIT);
end;

function TChunks.AddINVOKE: Integer;
begin
  result := EmitByte( OP_INVOKE);
end;

function TChunks.AddJUMP: Integer;
begin
 result := EmitByte(OP_JUMP);
end;

function TChunks.AddJUMP_IF_FALSE: Integer;
begin
  result := EmitByte(OP_JUMP_IF_FALSE);
end;

function TChunks.AddLESS: Integer;
begin
  result := EmitByte(OP_LESS);
end;

function TChunks.AddLESSTHANEQUAL: integer;
begin
  result := AddGreater;//FBytes.Add(ord(OP_GREATER));
  result := AddNot;//FBytes.Add(ord(OP_NOT));
end;

function TChunks.AddLOOP: Integer;
begin
  result := EmitByte(OP_LOOP);
end;

function TChunks.AddMETHOD: Integer;
begin
  result := EmitByte(OP_METHOD);
end;

function TChunks.AddMULTIPLY: Integer;
begin
  result := EmitByte(OP_MULTIPLY);
end;

function TChunks.AddNEGATE: Integer;
begin
  result := EmitByte(OP_NEGATE);
end;

function TChunks.AddNIL: Integer;
begin
  result := EmitByte(OP_NIL);
end;

function TChunks.AddNOT: Integer;
begin
  result := EmitByte(OP_NOT);
end;

function TChunks.AddPOP: Integer;
begin
  result := EmitByte(OP_POP);
end;

function TChunks.AddPRINT: Integer;
begin
  result := EmitByte(OP_PRINT);
end;

function TChunks.AddReturn : integer;
begin
  result := EmitByte(OP_RETURN);
end;



function TChunks.AddSET_LOCAL: Integer;
begin
  result := EmitByte(OP_SET_LOCAL);
end;

function TChunks.AddSET_PROPERTY: Integer;
begin
   result := EmitByte(OP_SET_PROPERTY);
end;

function TChunks.AddSET_UPVALUE: Integer;
begin

  result := EmitByte(OP_SET_UPVALUE);
end;


function TChunks.AddADD: Integer;
begin

  result := EmitByte(OP_ADD);
end;

function TChunks.AddSUBTRACT: Integer;
begin

  result := EmitByte(OP_SUBTRACT);
end;

function TChunks.AddSUPER_INVOKE: Integer;
begin

  result := EmitByte(OP_SUPER_INVOKE);
end;

function TChunks.AddTRUE: Integer;
begin

  result := EmitByte(OP_TRUE);
end;

function TChunks.BytesToString: TStrings;
var
  i : integer;
begin
  result := TStringList.create;

  for i := 0 to FBytes.Count-1 do
  begin
    result.add(Inttostr(pByte(FBytes[i])^));
  end;
end;


procedure TChunks.EmitConstant(const value : pValue);
var
  constantIdx : integer;
begin
  constantIdx := Constants.Add(value);
  EmitBytes(OP_CONSTANT,ConstantIdx);
end;




function TChunks.Count: Integer;
begin
  result := FBytes.Count;
end;

constructor TChunks.Create(const FunctionName : String);
begin
  FName := Functionname;
  FBytes := TList.create;
  FConstants := TConstants.Create(Fname);

end;

destructor TChunks.destroy;
begin
  FBytes.Free;
  FConstants.Free;
  inherited;
end;


procedure TChunks.DoChunk(const Operand: TOpCodes);
begin
  if assigned(FOnChunk) then FOnChunk(Operand);
end;

{ TConstants }

constructor TConstants.create(const functionName : String);
begin
  FName := FunctionName;
  FItems := TValueList.create(true);
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

procedure TConstants.setItem(const Index: integer; const Value: pValue);
begin
  FItems[index] := Value;
end;

end.
