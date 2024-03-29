unit ValueManager;

interface

uses

  classes,
  LoxTypes,
  Values;

type

  TValueDisposal = class
    procedure DisposeList(var value : TValueRecord);
    procedure DisposeNative(var value : TValueRecord);
    procedure DisposeString(var value : TValueRecord);
    procedure DisposeValue(var value : TValueRecord);
    procedure DisposeFunction(var value : TValueRecord);
    procedure DisposeLoxFunction(var value: pLoxFunction);
  end;

  TValueCreation = class
  private
    //fSize : Longint;

  public
    function NewValue(

      const Kind : TLoxKind;
      const Obj  : pointer) : TValueRecord;
    function NewLoxString(Const str : string) : pLoxString;
    function newLoxFunction(const Name : String) : pLoxFunction;
    function newValueFromFunction(functionObj : pLoxFunction) : TValueRecord;
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : TValueRecord;
    function NewNil : TValueRecord;
    function NewList(const name : string) : pLoxList;
    function newNative(const NativeFn : TNativeFunction) : TValueRecord;
    function NewNumber(Const number : TNumber) : TValueRecord;
    function NewBool(const Bool : Boolean) : TValueRecord;
//    function NewValues : TStack;
    function newValueFromList(const List : pLoxList) : TValueRecord;
    function newValueList(const name : string) : TValueRecord;
    function NewString(const txt : String) : TValueRecord;
//    property Size : LongInt read FSize;
  end;

  TNumberMemory = class
  const BUFFER_CAPACITY = 256;
  private
    FCreation : TValueCreation;
    FDisposal : TValueDisposal;
    FCount : integer;
    FStack : TStack;
    function getStackStock: integer;
  protected
    procedure AllocMemory;

  public
    function BorrowNumber : TValueRecord;
    procedure ReturnNumber(const value : TValueRecord);
    constructor create(
      const creation : TValueCreation;
      const disposal : TValueDisposal);
    destructor destroy; override;
    property StackTop : integer read getStackStock;
  end;



  // add in here the size of items borrowed. Then we can work out how much mem is being consumed.
  TValueManager = class
  private
    FLogger : TStrings;

    FNumberMemory : TNumberMemory;
    FOwnValues : boolean;
    FCompilerItems :  TStack; //TList;
    FValueFactory  :  TValueCreation;
    FValueDisposal :  TValueDisposal;
  private
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : TValueRecord; overload;


    function NewList(const name : string) : pLoxList;
    function NewValues : TStack;
//    function IndexOf(const value : TValueRecord) : integer;


    procedure SaveValue(const value : TValueRecord);
    function getCapacity: integer;


  public
    procedure FlushBuffer;
    procedure Dispose(var value : pLoxFunction); overload;
    procedure Dispose(var value : TValueRecord);overload;
    function newValueFromFunction(
      const functionObj : pLoxFunction) : TValueRecord;
    function newNative(const NativeFn : TNativeFunction) : TValueRecord;
    function NewFunction(const name : string) : TValueRecord; overload;
    function newValueList(const OwnValue : boolean; const name : string) : TValueRecord;
    function newLoxFunction(const Name : String) : pLoxFunction;

    function NewString(const txt : String) : TValueRecord;
    function NewNumber(Const number : TNumber) : TValueRecord;
    function NewBool(const Bool : Boolean) : TValueRecord;
    function NewNil : TValueRecord;


    property Logger : TStrings read FLogger write FLogger;
    property OwnValues : Boolean read fOwnValues write FOwnValues;
    property Capacity : integer read getCapacity;

    constructor create;
    destructor destroy; override;
  end;

    function getNumber(const ValueRecord : TValueRecord)  : TNumber;

    function BoolToString(const ValueRecord : TValueRecord)  : String;
    function getBoolean(const ValueRecord : TValueRecord)  : Boolean;


    function getObject(const ValueRecord : TValueRecord) : pLoxObject;
    function getFunction(const ValueRecord : TValueRecord) : pLoxFunction;
    function getNull(const ValueRecord : TValueRecord): boolean;

    function GetString(const ValueRecord : TValueRecord) : String;
    function getIsNumber(const ValueRecord : TValueRecord): Boolean;
    function getIsString(const ValueRecord : TValueRecord): Boolean;
    function getIsObject(const ValueRecord : TValueRecord) : Boolean;
    function getIsFunction(const ValueRecord : TValueRecord) : Boolean;
    function getIsNull(const ValueRecord : TValueRecord): Boolean;
    function getIsNative(const ValueRecord : TValueRecord): Boolean;
    function getNative(const ValueRecord : TValueRecord): pLoxNative;


    function getKind(const ValueRecord : TValueRecord): TLoxKind;

    function getIsList(const ValueRecord : TValueRecord): Boolean;
    function getList(const ValueRecord : TValueRecord): pLoxList;
    function getIsBoolean(const ValueRecord : TValueRecord): Boolean;
    function getLoxString(const ValueRecord : TValueRecord): pLoxString;



var
   bc : TValueManager;
  // Numbers : TNumberMemory;
  // Creation : TValueCreation;
  // Disposal : TValueDisposal;
implementation

uses
  sysutils;              

 

function TValueCreation.NewBool(const Bool : Boolean) : TValueRecord;
begin
//  new(result);
  Fillchar(Result,Sizeof(Result),#0);
  result.Kind := lxBoolean;
  result.Bool := Bool;
  //FCompilerItems.Add(result);
  //FSize := FSize + Sizeof(result^);
end;



function TValueCreation.NewNil: TValueRecord;
begin
  //new(result);
  Fillchar(Result,Sizeof(Result),#0);
  result.Kind := lxNull;
end;

function TValueCreation.NewNumber(const number : TNumber) : TValueRecord;
begin
  //new(result);
  Fillchar(Result,Sizeof(Result),#0);
  result.Kind := lxNumber;
  result.Number := Number;
  //FCompilerItems.Add(result);


end;


function TValueCreation.newValueFromList(const List : pLoxList) : TValueRecord;
begin
  //new(result);
  Fillchar(Result,Sizeof(Result),#0);
  result.Kind := lxList;
  result.Obj := List;
end;


function TValueCreation.newValueList(const name : string) : TValueRecord;
begin
  Fillchar(Result,Sizeof(Result),#0);
  result := newValueFromList(newList(name));

end;

function TValueCreation.NewList(const name : string) : pLoxList;
begin
(*  new(result);
  result.LoxObject.Kind := OBJ_LIST;
  result.LoxObject.Next := nil;
  result.Name := Name;

  result.Items.init; // := newValues;
  //FCompilerItems.Add(result); *)
end;

function TValueCreation.NewValue(const Kind: TLoxKind; const Obj: Pointer): TValueRecord;
begin
  FillChar(Result, SizeOf(result), #0);
  Result.Kind := Kind;
  Result.Obj := Obj;
end;

function TValueCreation.NewLoxString(const Str: String): pLoxString;
begin
  New(Result);
  FillChar(Result^,Sizeof(TLoxString),#0);
  result.Chars := Str;
end;

function TValueCreation.NewString(const Txt: String): TValueRecord;
var
  P: pLoxString;
begin
  if length(txt) <= TShortSize then
  begin
    FillChar(Result, SizeOf(result), #0);
    Result.Kind := lxShort;
    Result.str := txt;
    exit;
  end;
  P := NewLoxString(Txt);
  Result := NewValue(lxString, P);
end;



function TValueCreation.newValueFromFunction(functionObj: pLoxFunction): TValueRecord;
begin
//  new(result);
  Fillchar(Result,Sizeof(Result),#0);
  result.Kind := lxFunction;
  result.Obj := FunctionObj;
end;

function TValueCreation.newFunction(
  const prev : pLoxFunction;
  const Name : String;
  const Kind : TFunctionKind) : TValueRecord;
var
  fn : pLoxFunction;
begin
  Fillchar(Result,Sizeof(Result),#0);
  fn := newLoxFunction(name);
  //new(result);
  result.Kind := lxFunction;
  result.Obj := fn;
end;


function TValueCreation.newLoxFunction(const Name : String) : pLoxFunction;
begin
    new(result);
    result.LoxObject.Kind := OBJ_FUNCTION;
    result.LoxObject.Next := nil;
    result.FuncKind := Type_Function;
    result.Arity := 0;
    result.Name := Name;
    //result.Chunks := TChunks.Create(Name);//(Constants); //.Init;
    result.OpCodes.init;
    result.Constants.Init;
end;

function TValueCreation.newNative(const NativeFn : TNativeFunction) : TValueRecord;
var
  LoxNative : PLoxNative;
begin
 // new(result);
  Fillchar(Result,Sizeof(Result),#0);
  new(loxNative);
  loxNative.Native := NativeFn;

  result.Kind := lxNative;
  result.Obj := LoxNative;
end;

constructor TValueManager.create;
begin

  FCompilerItems.init;
//  FNumberMemory := TNumberMemory.create(FValueDisposal);
  FValueFactory  := TValueCreation.Create;
  FValueDisposal := TValueDisposal.Create;
end;

procedure TValueManager.FlushBuffer;
var
  i : integer;
  value : TValueRecord;
begin
  exit;
  for i := FCompilerItems.Capacity - 1 downto 0 do
  begin
    value := FCompilerItems.Pop;  //<=== this will fail as the capacity is bigger than the stack top
    //FCompilerItems.Remove(value);
    FValueDisposal.DisposeValue(value);
  end;

end;

destructor TValueManager.destroy;
begin
  flushBuffer;
  FNumberMemory.free;
  FValueFactory.free;

  FValueDisposal.Free;
  inherited;
end;


procedure TValueManager.Dispose(var value: pLoxFunction);
begin
  FValueDisposal.DisposeLoxFunction(Value);
end;

procedure TValueManager.Dispose(var value: TValueRecord);
begin
  FValueDisposal.DisposeValue(Value);
end;

function TValueManager.getCapacity: integer;
begin
  result := FCompilerItems.Capacity;
end;


(*
function TValueManager.IndexOf(const value: TValueRecord): integer;
begin
  result := FCompilerItems.IndexOf(Value);
end; *)

function TValueManager.NewBool(
  const Bool: Boolean): TValueRecord;
begin
  result := FValueFactory.NewBool(bool);
  saveValue(result);
end;

function TValueManager.newFunction(const name: string): TValueRecord;
begin
  result := newfunction(nil,Name,TYPE_FUNCTION);
end;

function TValueManager.newFunction(const prev: pLoxFunction; const Name: String;
  const Kind: TFunctionKind): TValueRecord;
begin
  result := FValueFactory.NewFunction(prev,Name,Kind);
  SaveValue(result);

end;

function TValueManager.NewList(
  const name: string): pLoxList;
begin
  result := FValueFactory.NewList(Name);

end;

function TValueManager.newLoxFunction(const Name: String): pLoxFunction;
begin
  result := FValueFactory.NewLoxFunction(Name);
end;

function TValueManager.newNative(
  const NativeFn: TNativeFunction): TValueRecord;
begin
   result := FValueFactory.NewNative(NativeFn);
   SaveValue(result);
end;

function TValueManager.NewNil: TValueRecord;
begin
  result := FValueFactory.NewNil;
  SaveValue(result);
end;

function TValueManager.NewNumber(
  const number: TNumber): TValueRecord;
begin
  result := FValueFactory.NewNumber(Number);
  SaveValue(result);
end;

function TValueManager.NewString(
  const txt: String): TValueRecord;
begin
  result := FValueFactory.NewString(txt);
  SaveValue(result);
end;

function TValueManager.newValueFromFunction(
  const functionObj: pLoxFunction): TValueRecord;
begin
  result := FValueFactory.NewValueFromFunction(functionObj);
//  result.Requester := Requester;
end;

function TValueManager.newValueList(
  const OwnValue : boolean;
  const name: string): TValueRecord;
begin
   result := FValueFactory.NewValueList(name);
   if OwnValue then FCompilerItems.Push(result);
end;

function TValueManager.NewValues: TStack;
begin

end;

procedure TValueManager.SaveValue(const value: TValueRecord);
begin

end;

procedure TValueDisposal.DisposeList(var value : TValueRecord);
var
  p : pLoxList;
begin
  p := pLoxList(Value.Obj);
//  p.Items.Free;
  dispose(p);
  value.Obj := nil;
end;

procedure TValueDisposal.DisposeLoxFunction(var value: pLoxFunction);
var

  i : integer;
  constant : TValueRecord;
begin
  //assert(value.Chunks.OwnsValues = false, 'the chunk values are owned - dispose will abort');

  for i := value.Constants.StackTop-1 downto 0 do
  begin
    constant := value.Constants.items[i];
    disposeValue(constant);
  end;
//  Value.Chunks.Free;
  Dispose(Value);
end;

procedure TValueDisposal.DisposeFunction(var value : TValueRecord);
begin
  if value.Kind <> lxFunction then raise exception.create('value for disposal is not a function');
  DisposeLoxFunction(pLoxFunction(Value.Obj));
  Value.Obj := nil;
end;


procedure TValueDisposal.disposeNative(var value : TValueRecord);
begin
  if value.Kind <> lxNative then raise exception.create('value for disposal is not a Native');
  Dispose(pLoxNative(Value.Obj));
  Value.Obj := nil;
end;

procedure TValueDisposal.disposeString(var value : TValueRecord);
begin
  if value.Kind <> lxString then raise exception.create('value for disposal is not a String');
  dispose(pLoxString(Value.obj));
  value.Obj := nil;
end;

procedure TValueDisposal.DisposeValue(var value : TValueRecord);
begin
  if GetIsBoolean(Value) or
     GetIsNull(Value) or
     GetIsNumber(Value) then exit; //all of these don't have pointers

  if GetIsList(Value) then
  begin
    DisposeList(Value);
    exit;
  end;

  if GetIsString(Value) then
  begin
    disposeString(value);
    exit;
  end;

  if GetIsNative(Value) then
  begin
     disposeNative(Value);
     exit;
  end;

  if GetIsFunction(Value) then
  begin
    DisposeFunction(value);
    exit;
  end;

end;


procedure setObject(var ValueRecord : TValueRecord;const value : pLoxObject);
begin
  ValueRecord.Kind := lxObject;
end;

procedure SetString(var ValueRecord : TValueRecord;const value: String);
var
  Obj : pLoxString;
begin
  Assert(getIsString(ValueRecord) = true, 'Value is not a string object');
  Obj := GetLoxString(ValueRecord);
  if Obj <> nil then
    Obj.Chars := Value;
end;

function  getObject(const ValueRecord : TValueRecord) : pLoxObject;
begin
  result := nil;
  if (ValueRecord.Kind = lxObject) or
     (ValueRecord.Kind = lxString) or
     (valuerecord.Kind = lxNative) or
     (valueRecord.Kind = lxfunction) or
     (valueRecord.Kind = lxList) then
    result := ValueRecord.Obj;
end;


function  BoolToString(const ValueRecord : TValueRecord) : String;
begin
  if getBoolean(ValueRecord) = true then result := 'True'
  else
    result := 'False';
end;

function GetString(const ValueRecord : TValueRecord): String;
var
  Obj : pLoxObject;

begin
  case  ValueRecord.Kind of
    lxObject : begin
       Obj := GetObject(ValueRecord);
       case Obj.Kind of
         OBJ_STRING : begin
           result := pLoxString(obj).Chars;
         end;
       end;
   end;

   lxShort    : begin
      result := ValueRecord.str;
   end;

   lxFunction : begin
       result := pLoxFunction(ValueRecord.Obj).Name;
   end;

   lxBoolean : begin
     result := BoolToString(ValueRecord);
   end;

   lxNumber : begin
     result := floatToStr(GetNumber(ValueRecord));
   end;

   lxNull  : begin
     result := 'Nil';
   end;

   lxString : begin
     result := pLoxString(ValueRecord.Obj).Chars;
   end;

  end;
end;

function  getBoolean(const ValueRecord : TValueRecord) : Boolean;
begin
  result := false;
  if  ValueRecord.Kind = lxBoolean then result := ValueRecord.Bool;

end;

function getFunction(const ValueRecord : TValueRecord): pLoxFunction;
begin
  result := nil;
  if ValueRecord.Kind = lxFunction then
     result := pLoxFunction(ValueRecord.Obj);
end;

procedure  setBoolean(var ValueRecord : TValueRecord; const value : Boolean);
begin
  assert(GetIsBoolean(ValueRecord), 'value is not a boolean so can''t set it to one');
  ValueRecord.Kind := lxBoolean;
  ValueRecord.Bool := Value;
end;

function  getIsBoolean(const ValueRecord : TValueRecord): Boolean;
begin
   result := ValueRecord.Kind = lxBoolean;
end;

function  getIsFunction(const ValueRecord : TValueRecord): Boolean;
begin
   result := ValueRecord.Kind = lxFunction;
end;

function  getIsList(const ValueRecord : TValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxList;
end;

function  getIsNative(const ValueRecord : TValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNative;
end;

function  getIsNull(const ValueRecord : TValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function  getIsNumber(const ValueRecord : TValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNumber;
end;

function  getIsObject(const ValueRecord : TValueRecord): Boolean;
begin
   result := (ValueRecord.Kind = lxObject)
end;

function  getIsString(const ValueRecord : TValueRecord): Boolean;
begin
  result :=  ValueRecord.Kind = lxString;//getisobject and (getObject.Kind = OBJ_STRING);
end;

function  getKind(const ValueRecord : TValueRecord): TLoxKind;
begin
  result := ValueRecord.Kind
end;

function getList(const ValueRecord : TValueRecord): pLoxList;
begin
  result := nil;
  if valueRecord.Kind = lxList then
    result := pLoxList(valueRecord.obj);
end;

function  getLoxString(const ValueRecord : TValueRecord): pLoxString;
begin
  result := nil;
  if ValueRecord.Kind = lxString then
    result := pLoxString(ValueRecord.obj);
end;

function  getNative(const ValueRecord : TValueRecord): pLoxNative;
begin
  result := nil;
  if ValueRecord.Kind = lxNative then
     result := pLoxNative(ValueRecord.Obj);
end;

Function  getNull(const ValueRecord : TValueRecord): boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function  getNumber(const ValueRecord : TValueRecord) : TNumber;
begin
  result := -1;
  if ValueRecord.Kind = lxNumber then
  begin
    result := ValueRecord.Number;
  end;
end;

procedure SetNumber(var ValueRecord : TValueRecord;const value : TNumber);
begin
   assert(GetIsNumber(ValueRecord), 'value is not a number so can''t set it to one');
   ValueRecord.Kind := lxNumber;
   ValueRecord.Number := Value;
end;

{ TNumberMemory }

procedure TNumberMemory.AllocMemory;
var
  i : integer;
  Num : TValueRecord;
begin
  for i := 0 to BUFFER_CAPACITY-1 do
  begin
    Num := FCreation.NewNumber(0);
    FStack.Push(Num);
  end;
end;

function TNumberMemory.BorrowNumber: TValueRecord;
begin
  if FStack.StackTop <= 0 then raise exception.create('no more mem');
  result := FStack.pop;
end;

constructor TNumberMemory.create(
  const creation : TValueCreation;
  const disposal : TValueDisposal);
begin
  FCreation := creation;
  FDisposal := Disposal;
  FStack.Init;
  AllocMemory;
  FCount := 0;
end;

destructor TNumberMemory.destroy;
var
  i : integer;
  val : TValueRecord;
begin
  for i := 0 to FStack.StackTop-1 do
  begin
    Val := FStack.Pop;
   // FDisposal.DisposeNumber(Val);
  end;
//  FStack.Free;
  inherited;
end;

function TNumberMemory.getStackStock: integer;
begin
  result := FStack.StackTop;
end;

procedure TNumberMemory.ReturnNumber(const value: TValueRecord);
begin
  FStack.Push(Value);
end;

initialization
   bc := TValueManager.Create;
   bc.OwnValues := False;


  // Creation := TValueCreation.Create;
 //  Disposal := TValueDisposal.create;


 //  Numbers := TNumberMemory.create(creation,disposal);

finalization

   bc.free;
 //  Numbers.free;
 //  creation.free;
 //  disposal.free;

end.


