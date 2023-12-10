unit ValueManager;

interface

uses

  classes,
  LoxTypes,
  Values;

type

  (* whilst I generally think this is on the right track,
     there is a lot of memory fragmentation happening here.

     to understand more fully this issue, I will endeavour to set limits on the
     amount of memory available in any lox program.

     I will do that by setting limits on, for example, the number of functions,
     the number of "numbers" etc,

     basically, what I'm gonna do, is grab memory up-front, and then allocate from
     memory pools of a reasonable size. I will probably allow multiple resize up to a point though
     so the limit will be based on max capacity, with a minimum stipulated as well.

     so that's not gonna be easy...

     but it will hopefully mean that after a program runs, the memory is a known
     entity.

     What would be a reasonable "chunk" of memory to attempt to grab?

     Answer : I have no idea.

     However, we can also add in some code to allow a user to set these limits.

     This is before we get to the Garbage collection of memory btw.

     it's on the radar, and will be done asap.

   *)


  TValueDisposal = class
    procedure DisposeList(var List : pValueRecord);
    procedure DisposeNil(var value : pValueRecord);
    procedure DisposeBoolean(var value : pValueRecord);
    procedure DisposeNumber(var value : pValueRecord);
    procedure DisposeNative(var native : pValueRecord);
    procedure DisposeString(var str : pValueRecord);
    procedure DisposeValue(var value : pValueRecord);
    procedure DisposeFunction(var value : pValueRecord);
    procedure DisposeLoxFunction(var value: pLoxFunction);
  end;

  TValueCreation = class
  private
    //fSize : Longint;

  public
    function NewValue(
      const requester : TRequester;
      const Kind : TLoxKind;
      const Obj  : pointer) : pValueRecord;
    function NewLoxString(Const str : string) : pLoxString;
    function newLoxFunction(const Name : String) : pLoxFunction;
    function newValueFromFunction(functionObj : pLoxFunction) : pValueRecord;
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : pValueRecord;
    function NewNil : pValueRecord;
    function NewList(const name : string) : pLoxList;
    function newNative(const NativeFn : TNativeFunction) : pValueRecord;
    function NewNumber(Const number : TNumber) : pValueRecord;
    function NewBool(const Bool : Boolean) : pValueRecord;
    function NewValues : TValueList;
    function newValueFromList(const List : pLoxList) : pValueRecord;
    function newValueList(const name : string) : pValueRecord;
    function NewString(const requester : TRequester;const txt : String) : pValueRecord;
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
    function BorrowNumber : pValueRecord;
    procedure ReturnNumber(const value : pValueRecord);
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
    FItems : TList;
    FValueFactory  :  TValueCreation;
    FValueDisposal :  TValueDisposal;
  private
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : pValueRecord; overload;


    function NewList(const name : string) : pLoxList;
    function NewValues : TValueList;
    function IndexOf(const value : pValueRecord) : integer;
    function getCount: integer;

    procedure SaveValue(const value : pValueRecord);

  public
    procedure FlushBuffer;
    function newValueFromFunction(functionObj : pLoxFunction) : pValueRecord;
    function newNative(const NativeFn : TNativeFunction) : pValueRecord;
    function NewFunction(const name : string) : pValueRecord; overload;
    function newValueList(const OwnValue : boolean; const name : string) : pValueRecord;
    function newLoxFunction(const Name : String) : pLoxFunction;
    procedure Dispose(var value : pLoxFunction); overload;
    procedure Dispose(var value : pValueRecord);overload;
    function NewString(const requester : TRequester;const txt : String) : pValueRecord;
    function NewNumber(Const number : TNumber) : pValueRecord;
    function NewBool(const Bool : Boolean) : pValueRecord;
    function NewNil : pValueRecord;


    property Logger : TStrings read FLogger write FLogger;
    property OwnValues : Boolean read fOwnValues write FOwnValues;
    property Count : integer read getCount;

    constructor create;
    destructor destroy; override;
  end;







    function getNumber(const ValueRecord : pValueRecord)  : TNumber;
    procedure SetNumber(const ValueRecord : pValueRecord; const value : TNumber);
    function BoolToString(const ValueRecord : pValueRecord)  : String;
    function getBoolean(const ValueRecord : pValueRecord)  : Boolean;
    procedure setBoolean(const ValueRecord : pValueRecord;const value : Boolean);
    procedure setObject(const ValueRecord : pValueRecord;const value : pLoxObject);
    function  getObject(const ValueRecord : pValueRecord) : pLoxObject;
    function  getFunction(const ValueRecord : pValueRecord) : pLoxFunction;
    procedure setNull(const ValueRecord : pValueRecord;const value : Boolean);
    function getNull(const ValueRecord : pValueRecord): boolean;
    procedure SetString(const ValueRecord : pValueRecord;Const value : String);
    function GetString(const ValueRecord : pValueRecord) : String;
    function getIsNumber(const ValueRecord : pValueRecord): Boolean;
    function getIsString(const ValueRecord : pValueRecord): Boolean;
    function getIsObject(const ValueRecord : pValueRecord) : Boolean;
    function getIsFunction(const ValueRecord : pValueRecord) : Boolean;
    function getIsNull(const ValueRecord : pValueRecord): Boolean;
    function getIsNative(const ValueRecord : pValueRecord): Boolean;
    function getNative(const ValueRecord : pValueRecord): pLoxNative;
    procedure setNative(const ValueRecord : pValueRecord;const Value: pLoxNative);

    function getKind(const ValueRecord : pValueRecord): TLoxKind;
    procedure setKind(const ValueRecord : pValueRecord; const Value: TLoxKind);
    function getIsList(const ValueRecord : pValueRecord): Boolean;
    function getList(const ValueRecord : pValueRecord): pLoxList;
    function getIsBoolean(const ValueRecord : pValueRecord): Boolean;
    function getLoxString(const ValueRecord : pValueRecord): pLoxString;



var
   BorrowChecker : TValueManager;
  // Numbers : TNumberMemory;
  // Creation : TValueCreation;
  // Disposal : TValueDisposal;
implementation
uses
  sysutils;              

 

function TValueCreation.NewBool(const Bool : Boolean) : pValueRecord;
begin
  new(result);
  result.Kind := lxBoolean;
  result.Bool := Bool;
  //FItems.Add(result);
  //FSize := FSize + Sizeof(result^);
end;

function TValueCreation.NewValues: TValueList;
begin
  result := TValueList.Create(false);
end;

function TValueCreation.NewNil: pValueRecord;
begin
  new(result);
  result.Kind := lxNull;
end;

function TValueCreation.NewNumber(const number : TNumber) : pValueRecord;
begin
  new(result);
  result.Kind := lxNumber;
  result.Number := Number;
  //FItems.Add(result);


end;


function TValueCreation.newValueFromList(const List : pLoxList) : pValueRecord;
begin
  new(result);
  result.Kind := lxList;
  result.Obj := List;
end;


function TValueCreation.newValueList(const name : string) : pValueRecord;
begin
  result := newValueFromList(newList(name));

end;

function TValueCreation.NewList(const name : string) : pLoxList;
begin
  new(result);
  result.LoxObject.Kind := OBJ_LIST;
  result.LoxObject.Next := nil;
  result.Name := Name;

  result.Items := newValues;
  //FItems.Add(result);
end;

function TValueCreation.NewValue(const Requester: TRequester; const Kind: TLoxKind; const Obj: Pointer): pValueRecord;
begin
  New(Result);
  FillChar(Result^, SizeOf(TValueRecord), #0);
  Result.Requester := Requester;
  Result.Kind := Kind;
  Result.Obj := Obj;
end;

function TValueCreation.NewLoxString(const Str: String): pLoxString;
begin
  New(Result);
  Result.Init;
  Result.Chars := Str;
end;

function TValueCreation.NewString(const Requester: TRequester; const Txt: String): pValueRecord;
var
  P: pLoxString;
begin
  P := NewLoxString(Txt);
  Result := NewValue(Requester, lxString, P);
end;



function TValueCreation.newValueFromFunction(functionObj: pLoxFunction): pValueRecord;
begin
  new(result);
  result.Kind := lxFunction;
  result.Obj := FunctionObj;
end;

function TValueCreation.newFunction(
  const prev : pLoxFunction;
  const Name : String;
  const Kind : TFunctionKind) : pValueRecord;
var
  fn : pLoxFunction;
begin
  fn := newLoxFunction(name);
  new(result);
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
    result.Chunks := TChunks.Create(Name);//(Constants); //.Init;
end;




function TValueCreation.newNative(const NativeFn : TNativeFunction) : pValueRecord;
var
  LoxNative : PLoxNative;
begin
  new(result);
  new(loxNative);
  loxNative.Native := NativeFn;

  result.Kind := lxNative;
  result.Obj := LoxNative;
end;

constructor TValueManager.create;
begin

  FItems := TList.Create;
//  FNumberMemory := TNumberMemory.create(FValueDisposal);
  FValueFactory  := TValueCreation.Create;
  FValueDisposal := TValueDisposal.Create;
end;

procedure TValueManager.FlushBuffer;
var
  i : integer;
  value : pValueRecord;
begin

  for i := FItems.Count - 1 downto 0 do
  begin
    value := FItems[i];
    FItems.Remove(value);
    FValueDisposal.DisposeValue(value);
  end;
 
end;

destructor TValueManager.destroy;
begin
  flushBuffer;
  FNumberMemory.free;
  FValueFactory.free;
  FItems.Free;
  FValueDisposal.Free;
  inherited;
end;


procedure TValueManager.Dispose(var value: pLoxFunction);
begin
  FValueDisposal.DisposeLoxFunction(Value);
end;

procedure TValueManager.Dispose(var value: pValueRecord);
begin
  FValueDisposal.DisposeValue(Value);
end;

function TValueManager.getCount: integer;
begin
  result := FItems.Count;
end;


function TValueManager.IndexOf(const value: pValueRecord): integer;
begin
  result := FItems.IndexOf(Value);
end;

function TValueManager.NewBool(
  const Bool: Boolean): pValueRecord;
begin
  result := FValueFactory.NewBool(bool);
  saveValue(result);
end;

function TValueManager.newFunction(const name: string): pValueRecord;
begin
  result := newfunction(nil,Name,TYPE_FUNCTION);
end;

function TValueManager.newFunction(const prev: pLoxFunction; const Name: String;
  const Kind: TFunctionKind): pValueRecord;
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
  const NativeFn: TNativeFunction): pValueRecord;
begin
   result := FValueFactory.NewNative(NativeFn);
   SaveValue(result);
end;

function TValueManager.NewNil: pValueRecord;
begin
  result := FValueFactory.NewNil;
  SaveValue(result);
end;

function TValueManager.NewNumber(
  const number: TNumber): pValueRecord;
begin
  result := FValueFactory.NewNumber(Number);
  SaveValue(result);
end;

function TValueManager.NewString(
  const requester : TRequester;const txt: String): pValueRecord;
begin
  result := FValueFactory.NewString(requester,txt);
  SaveValue(result);
end;

function TValueManager.newValueFromFunction(functionObj: pLoxFunction): pValueRecord;
begin
  result := FValueFactory.NewValueFromFunction(functionObj);
end;

function TValueManager.newValueList(
  const OwnValue : boolean;
  const name: string): pValueRecord;
begin
   result := FValueFactory.NewValueList(name);
   if OwnValue then FItems.Add(result);
end;

function TValueManager.NewValues: TValueList;
begin

end;

procedure TValueManager.SaveValue(const value: pValueRecord);
begin
  if FOwnValues then
     FItems.add(value);
end;

procedure TValueDisposal.DisposeList(var List : pValueRecord);
var
  p : pLoxList;
begin
  p := pLoxList(List.Obj);
  p.Items.Free;
  dispose(p);
  p := nil;
  dispose(List);
  list := nil;
end;

procedure TValueDisposal.DisposeLoxFunction(var value: pLoxFunction);
var
  i : integer;
  constant : pValueRecord;
begin
  assert(value.Chunks.OwnsValues = false, 'the chunk values are owned - dispose will abort');

  for i := value.Chunks.ConstantCount-1 downto 0 do
  begin
    constant := value.Chunks.Constant[i];
    disposeValue(constant);
  end;
  Value.Chunks.Free;
  dispose(value);
  Value := nil;
end;

procedure TValueDisposal.DisposeFunction(var value : pValueRecord);
var
  i : integer;
  func : pLoxFunction;
  constant : pValueRecord;
begin
  if value.Kind <> lxFunction then raise exception.create('value for disposal is not a function');
  func := getFunction(Value);
  DisposeLoxFunction(func);
  Dispose(Value);
end;


procedure TValueDisposal.disposeNative(var native : pValueRecord);
var
  LoxNative : PLoxNative;
begin
  if Native.Kind <> lxNative then raise exception.create('value for disposal is not a Native');

  LoxNative :=  getNative(Native);
  Dispose(LoxNative);
  LoxNative := nil;
  Dispose(Native);
  Native := nil;
end;

procedure TValueDisposal.disposeString(var str : pValueRecord);
var
  p : pLoxString;
begin
  if str.Kind <> lxString then raise exception.create('value for disposal is not a String');

  p :=  GetLoxString(Str);
  dispose(p);
  p := nil;
  dispose(str);
  str := nil;
end;


procedure TValueDisposal.DisposeBoolean(var value : pValueRecord);
begin
  dispose(value);
  Value := nil;
end;

procedure TValueDisposal.DisposeNil(var value: pValueRecord);
begin
  dispose(Value);
  Value := nil;
end;

procedure TValueDisposal.DisposeNumber(var value : pValueRecord);
begin
   dispose(value);
   Value := nil;
end;

procedure TValueDisposal.DisposeValue(var value : pValueRecord);
begin
  if GetIsNull(Value) then
  begin
    DisposeNil(Value);
    exit;
  end;

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

  if GetIsNumber(Value) then
  begin
    disposeNumber(value);
    exit;
  end;

  if GetIsNative(Value) then
  begin
     disposeNative(Value);
     exit;
  end;

  if GetIsBoolean(Value) then
  begin
    disposeBoolean(Value);
    exit;
  end;

  if GetIsFunction(Value) then
  begin
    DisposeFunction(value);
    exit;
  end;

end;

procedure setNative(const ValueRecord : pValueRecord; const Value: pLoxNative);
begin
  ValueRecord.Kind := lxNative;
  ValueRecord.Obj := Value;
end;

procedure  setNull(const ValueRecord : pValueRecord;const value : Boolean);
begin
  ValueRecord.Kind := lxNull;
end;

procedure  setObject(const ValueRecord : pValueRecord;const value : pLoxObject);
begin
  ValueRecord.Kind := lxObject;
end;

procedure SetString(const ValueRecord : pValueRecord;const value: String);
var
  Obj : pLoxString;
begin
  Assert(getIsString(ValueRecord) = true, 'Value is not a string object');
  Obj := GetLoxString(ValueRecord);
  if Obj <> nil then
    Obj.Chars := Value;
end;

function  getObject(const ValueRecord : pValueRecord) : pLoxObject;
begin
  result := nil;
  if (ValueRecord.Kind = lxObject) or
     (ValueRecord.Kind = lxString) or
     (valuerecord.Kind = lxNative) or
     (valueRecord.Kind = lxfunction) or
     (valueRecord.Kind = lxList) then
    result := ValueRecord.Obj;
end;


function  BoolToString(const ValueRecord : pValueRecord) : String;
begin
  if getBoolean(ValueRecord) = true then result := 'True'
  else
    result := 'False';

end;

function GetString(const ValueRecord : pValueRecord): String;
var
  Obj : pLoxObject;
  fun : pLoxFunction;
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

   lxFunction : begin
       fun := getFunction(ValueRecord);
       result := fun^.name;

   end;

   lxBoolean : begin
     result := BoolToString(ValueRecord);
   end;

   lxNumber : begin
     result := floatToStr(GetNumber(ValueRecord));
   end;

   lxNull  : begin
     result := 'null';
   end;

   lxString : begin
     result := pLoxString(ValueRecord.Obj).Chars;
   end;

  end;
end;

function  getBoolean(const ValueRecord : pValueRecord) : Boolean;
begin
  result := false;
  if  ValueRecord.Kind = lxBoolean then result := ValueRecord.Bool;

end;

function  getFunction(const ValueRecord : pValueRecord): pLoxFunction;
begin
  result := nil;
  if  ValueRecord.Kind = lxFunction then
     result := pLoxFunction(ValueRecord.Obj);
end;

procedure  setBoolean(const ValueRecord : pValueRecord; const value : Boolean);
begin
  assert(GetIsBoolean(ValueRecord), 'value is not a boolean so can''t set it to one');
  ValueRecord.Kind := lxBoolean;
  ValueRecord.Bool := Value;
end;

procedure  setKind(const ValueRecord : pValueRecord; const Value: TLoxKind);
begin
  ValueRecord.Kind := Value;
end;

function  getIsBoolean(const ValueRecord : pValueRecord): Boolean;
begin
   result := ValueRecord.Kind = lxBoolean;
end;

function  getIsFunction(const ValueRecord : pValueRecord): Boolean;
begin
   result := ValueRecord.Kind = lxFunction;
end;

function  getIsList(const ValueRecord : pValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxList;
end;

function  getIsNative(const ValueRecord : pValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNative;
end;

function  getIsNull(const ValueRecord : pValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function  getIsNumber(const ValueRecord : pValueRecord): Boolean;
begin
  result := ValueRecord.Kind = lxNumber;
end;

function  getIsObject(const ValueRecord : pValueRecord): Boolean;
begin
   result := (ValueRecord.Kind = lxObject)
end;

function  getIsString(const ValueRecord : pValueRecord): Boolean;
begin
  result :=  ValueRecord.Kind = lxString;//getisobject and (getObject.Kind = OBJ_STRING);
end;

function  getKind(const ValueRecord : pValueRecord): TLoxKind;
begin
  result := ValueRecord.Kind
end;

function getList(const ValueRecord : pValueRecord): pLoxList;
begin
  result := nil;
  if valueRecord.Kind = lxList then
    result := pLoxList(valueRecord.obj);
end;

function  getLoxString(const ValueRecord : pValueRecord): pLoxString;
begin
  result := nil;
  if ValueRecord.Kind = lxString then
    result := pLoxString(ValueRecord.obj);
end;

function  getNative(const ValueRecord : pValueRecord): pLoxNative;
begin
  result := nil;
  if ValueRecord.Kind = lxNative then
     result := pLoxNative(ValueRecord.Obj);
end;

Function  getNull(const ValueRecord : pValueRecord): boolean;
begin
  result := ValueRecord.Kind = lxNull;
end;

function  getNumber(const ValueRecord : pValueRecord) : TNumber;
begin
  result := -1;
  if ValueRecord.Kind = lxNumber then
  begin
    result := ValueRecord.Number;
  end;
end;

procedure SetNumber(const ValueRecord : pValueRecord;const value : TNumber);
begin
   assert(GetIsNumber(ValueRecord), 'value is not a number so can''t set it to one');
   ValueRecord.Kind := lxNumber;
   ValueRecord.Number := Value;

end;



{ TNumberMemory }

procedure TNumberMemory.AllocMemory;
var
  i : integer;
  Num : pValueRecord;
begin
  for i := 0 to BUFFER_CAPACITY-1 do
  begin
    Num := FCreation.NewNumber(0);
    FStack.Push(Num);
  end;
end;

function TNumberMemory.BorrowNumber: pValueRecord;
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
  FStack := TStack.Create;
  AllocMemory;
  FCount := 0;
end;

destructor TNumberMemory.destroy;
var
  i : integer;
  val : pValueRecord;
begin
  for i := 0 to FStack.StackTop-1 do
  begin
    Val := FStack.Pop;
    FDisposal.DisposeNumber(Val);
  end;
  FStack.Free;
  inherited;
end;

function TNumberMemory.getStackStock: integer;
begin
  result := FStack.StackTop;
end;

procedure TNumberMemory.ReturnNumber(const value: pValueRecord);
begin
  FStack.Push(Value);
end;

initialization
   BorrowChecker := TValueManager.Create;
   BorrowChecker.OwnValues := False;


  // Creation := TValueCreation.Create;
 //  Disposal := TValueDisposal.create;


 //  Numbers := TNumberMemory.create(creation,disposal);

finalization

   BorrowChecker.free;
 //  Numbers.free;
 //  creation.free;
 //  disposal.free;

end.


