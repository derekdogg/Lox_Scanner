unit Stacks;

interface
uses
  LOXTypes,
  IntegerArray,
  ByteArray,
  ValueArray,
  ByteCodesArray;
type

  TIntegerStack = record
  private
    FIndex    : integer;
    FItems    : TIntegers;
    pStackTop : pInteger;
  public
    procedure Push(const Item : integer);
    function  Pop : Integer;
    procedure Init;
    procedure Finalize;
  end;

  TByteStack = record
  private
    FIndex    : Integer;
    FItems    : TBytes;
    pStackTop : pByte;
  public
    procedure Push(const Item : TOpCodes);
    function  Pop : Byte;
    procedure Init;
    procedure Finalize;
  end;

  (*TDoubleStack = record
  private
    FIndex    : Integer;
    FItems    : TBytes;
    pStackTop : pByte;
  public
    procedure Push(const Item : byte);
    function  Pop : Byte;
    procedure Init;
    procedure Finalize;
  end; *)

  (*TByteCodeStack = record
  private
    FCount    : Integer;
    FItems    : TByteCodes;

  public
    Function Count : Integer;
    function Top : pByteCode;
    Function Peek : pByteCode; overload;
    Function Peek(const distance : integer) : pByteCode; overload;
    procedure Push(const Item : TByteCode);
    function  Pop : TByteCode;
    procedure Init;
    procedure Finalize;

  end;  *)


implementation
uses
  sysutils;

{ TIntegerStack }

procedure TIntegerStack.Finalize;
begin
  FItems.Finalize;
end;

procedure TIntegerStack.Init;
begin
  FIndex := 0;
  FItems.Init;
  pStackTop := FItems.Item(0);
end;

function TIntegerStack.Pop: Integer;
begin
  result := -1;
  if FIndex = 0 then raise exception.create('Nothing to pop');
  dec(FIndex);
  dec(pStackTop);
  result := pStackTop^;
end;

procedure TIntegerStack.Push(const Item: integer);
begin
  inc(FIndex);
  FItems.Add(Item);
  inc(pStackTop);
end;

{ TByteStack }

procedure TByteStack.Finalize;
begin
  FItems.Finalize;
end;

procedure TByteStack.Init;
begin
  FIndex := 0;
  FItems.Init(16);
  pStackTop := FItems.Item(0);
end;

function TByteStack.Pop: Byte;
begin
  if FIndex = 0 then raise exception.create('Nothing to pop');
  dec(FIndex);
  dec(pStackTop);
  result := pStackTop^;
end;

procedure TByteStack.Push(const Item: TOpCodes);
begin
  inc(FIndex);
  FItems.Add(Item);
  inc(pStackTop);
end;

{TByteCodeStack}(*

function TByteCodeStack.Count: Integer;
begin
  result := FCount;
end;

procedure TByteCodeStack.Finalize;
begin
  FItems.Finalize;
end;

procedure TByteCodeStack.Init;
begin
  FCount := 0;
  FItems.Init;
end;

function TByteCodeStack.Peek(const distance: integer): pByteCode;
begin
  //return vm.stackTop[-1 - distance];
  result := FItems.GetItem(FCount-1 -distance);
end;

function TByteCodeStack.Peek: pByteCode;
begin
  result := Peek(0);
end;

function TByteCodeStack.Pop: TByteCode;
var
  removed : PByteCode;
begin

  if FCount = 0 then raise exception.create('Nothing to pop');
  result := Peek(0)^;

  if FItems.Remove = nil then raise exception.create('Failure to remove items from stack');
  Dec(FCount);
end;
           // 3 count compare to 1 count;
procedure TByteCodeStack.Push(const Item: TByteCode);
begin

  inc(FCount);
  FItems.Add(Item); //<==== the indexes get out of synch because the stack index and the array index are wildly different.
end;










function TByteCodeStack.Top: pByteCode;
begin
  result := peek(0);
end;  *)

end.

