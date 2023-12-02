unit ValueManager;

interface

uses
  classes,
  LoxTypes,
  Values;

type

  TValueDisposal = class
    procedure DisposeList(var List : pValue);
    procedure DisposeBoolean(var value : pValue);
    procedure DisposeNumber(var value : pValue);
    procedure disposeNative(var native : pValue);
    procedure DisposeString(var str : pValue);
    procedure DisposeValue(var value : pValue);
    procedure DisposeFunction(var value : pvalue); overload;
    procedure DisposeFunction(var value: pLoxFunction); overload;
  end;

  TValueCreation = class
    function newLoxFunction(const Name : String) : pLoxFunction;
    function newValueFromFunction(functionObj : pLoxFunction) : pValue;
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : pValue;
    function NewList(const name : string) : pLoxList;
    function newNative(const NativeFn : TNativeFunction) : pValue;
    function NewNumber(Const number : TNumber) : pValue;
    function NewBool(const Bool : Boolean) : pValue;
    function NewValues : TValueList;
    function newValueFromList(const List : pLoxList) : pValue;
    function newValueList(const name : string) : pValue;
    function NewString(const txt : String) : pValue;
  end;


  TValueManager = class
  private
    FItems : TList;
    FValueFactory  :  TValueCreation;
    FValueDisposal :  TValueDisposal;
  private
    function newFunction(
        const prev : pLoxFunction;
        const Name : String;
        const Kind : TFunctionKind) : pValue; overload;


    function NewList(const name : string) : pLoxList;
    function NewValues : TValueList;

     function IndexOf(const value : pValue) : integer;

  public
    function newValueFromFunction(functionObj : pLoxFunction) : pValue;
    function newNative(const NativeFn : TNativeFunction) : pValue;
    function NewFunction(const name : string) : pValue; overload;
    function newValueList(const name : string) : pValue;
    function newLoxFunction(const Name : String) : pLoxFunction;
    procedure Dispose(var value : pLoxFunction); overload;
    procedure Dispose(var value : pValue);overload;
    function NewString(const txt : String) : pValue;
    function NewNumber(Const number : TNumber) : pValue;
    function NewBool(const Bool : Boolean) : pValue;
    constructor create;
    destructor destroy; override;
  end;

var
   BorrowChecker : TValueManager;


implementation
uses
  sysutils;


function TValueCreation.NewBool(const Bool : Boolean) : pValue;
begin
  new(result);
  result.Kind := lxBoolean;
  result.Boolean := Bool;
  //FItems.Add(result);
end;

function TValueCreation.NewValues: TValueList;
begin
  result := TValueList.Create(false);
end;

function TValueCreation.NewNumber(const number : TNumber) : pValue;
begin
  new(result);
  result.Kind := lxNumber;
  result.Number := Number;
  //FItems.Add(result);
end;


function TValueCreation.newValueFromList(const List : pLoxList) : pValue;
begin
  new(result);
  result.Kind := lxList;
  result.ValueRecord.Obj := List;
end;


function TValueCreation.newValueList(const name : string) : pValue;
begin
  result := newValueFromList(newList(name));
  //FItems.Add(result);

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


function TValueCreation.NewString(const txt : String) : pValue;
var
  str : pLoxString;
begin
  new(result);
  str := NewLoxString(txt);
  result.Kind := lxString;
  result.ValueRecord.Obj := str;
  //FItems.Add(result);
end;




function TValueCreation.newValueFromFunction(functionObj: pLoxFunction): pValue;
begin
  new(result);
  result.Kind := lxFunction;
  result.valueRecord.Obj := FunctionObj;
end;

function TValueCreation.newFunction(
  const prev : pLoxFunction;
  const Name : String;
  const Kind : TFunctionKind) : pValue;
var
  fn : pLoxFunction;
begin
  fn := newLoxFunction(name);
  new(result);
  result.Kind := lxFunction;
  result.valueRecord.Obj := fn;
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




function TValueCreation.newNative(const NativeFn : TNativeFunction) : pValue;
var
  LoxNative : PLoxNative;
begin
  new(result);
  new(loxNative);
  loxNative.Native := NativeFn;

  result.Kind := lxNative;
  result.valueRecord.Obj := LoxNative;
 // FItems.Add(result);
end;

constructor TValueManager.create;
begin
  FItems := TList.Create;
  FValueFactory  := TValueCreation.Create;
  FValueDisposal := TValueDisposal.Create;
end;

destructor TValueManager.destroy;
var
  i : integer;
  value : pValue;
begin
  FValueFactory.free;
  (*for i := FItems.Count - 1 downto 0 do
  begin
    //FValueDisposal.DisposeFunction(FItems[i]);
    value := FItems[i];
    FValueDisposal.DisposeValue(value);
  end; *)

  FItems.Free;
  FValueDisposal.Free;
  inherited;
end;


procedure TValueManager.Dispose(var value: pLoxFunction);
begin
  FValueDisposal.DisposeFunction(Value);
end;

procedure TValueManager.Dispose(var value: pValue);
begin
  FValueDisposal.DisposeValue(Value);
end;

function TValueManager.IndexOf(const value: pValue): integer;
begin
  result := FItems.IndexOf(Value);
end;

function TValueManager.NewBool(
  const Bool: Boolean): pValue;
begin
  result := FValueFactory.NewBool(bool);
  //FItems.Add(result);
end;

function TValueManager.newFunction(const name: string): pValue;
begin
  result := newfunction(nil,Name,TYPE_FUNCTION);
end;

function TValueManager.newFunction(const prev: pLoxFunction; const Name: String;
  const Kind: TFunctionKind): pValue;
begin
  result := FValueFactory.NewFunction(prev,Name,Kind);
  //FItems.Add(result); //add to the root

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
  const NativeFn: TNativeFunction): pValue;
begin
   result := FValueFactory.NewNative(NativeFn);
end;

function TValueManager.NewNumber(
  const number: TNumber): pValue;
begin
  result := FValueFactory.NewNumber(Number);
  //FItems.Add(result);
end;

function TValueManager.NewString(
  const txt: String): pValue;
begin
  result := FValueFactory.NewString(txt);
  //FItems.add(result);
end;

function TValueManager.newValueFromFunction(functionObj: pLoxFunction): pValue;
begin
  result := FValueFactory.NewValueFromFunction(functionObj);
end;

function TValueManager.newValueList(
  const name: string): pValue;
begin
   result := FValueFactory.NewValueList(name);
   //FItems.Add(result);
end;

function TValueManager.NewValues: TValueList;
begin

end;

procedure TValueDisposal.DisposeList(var List : pValue);
var
  p : pLoxList;
begin
  p := pLoxList(List.ValueRecord.Obj);
  p.Items.Free;
  dispose(p);
  dispose(List);
end;

procedure TValueDisposal.DisposeFunction(var value: pLoxFunction);
var
  i : integer;
  constant : pValue;
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

procedure TValueDisposal.DisposeFunction(var value : pValue);
var
  i : integer;
  func : pLoxFunction;
  constant : pValue;
begin
  if value.Kind <> lxFunction then raise exception.create('value for disposal is not a function');
  func := Value.LoxFunction;
  DisposeFunction(func);
  Dispose(Value);
end;


procedure TValueDisposal.disposeNative(var native : pValue);
var
  LoxNative : PLoxNative;
begin
  if Native.Kind <> lxNative then raise exception.create('value for disposal is not a Native');

  LoxNative :=  Native.NativeFunction;
  Dispose(LoxNative);
  Dispose(Native);
end;




procedure TValueDisposal.disposeString(var str : pValue);
var
  p : pLoxString;
begin
  if str.Kind <> lxString then raise exception.create('value for disposal is not a String');

  p :=  str.LoxString;
  dispose(p);
  p := nil;
  dispose(str);
  str := nil;
end;


procedure TValueDisposal.DisposeBoolean(var value : pValue);
begin
  dispose(value);
end;

procedure TValueDisposal.DisposeNumber(var value : pValue);
begin
   dispose(value);
end;

procedure TValueDisposal.DisposeValue(var value : pValue);
begin
    if value.IsNull then
    begin
      //what goes here? DisposeNull(Value);
      exit;
    end;

  if value.IsList then
  begin
    DisposeList(Value);
    exit;
  end;

  if value.IsString then
  begin
    disposeString(value);
    exit;
  end;

  if value.IsNumber then
  begin
    disposeNumber(value);
    exit;
  end;

  if value.IsNative then
  begin
     disposeNative(Value);
     exit;
  end;

  if value.IsBoolean then
  begin
    disposeBoolean(Value);
    exit;
  end;

  if value.IsFunction then
  begin
    DisposeFunction(value);
    exit;
  end;

end;




initialization
   BorrowChecker := TValueManager.Create;

finalization
   BorrowChecker.free;


end.


