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
    procedure Push(const Item : byte);
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

  TByteCodeStack = record
  private
    FCount    : Integer;
    FIndex    : Integer;
    FItems    : TByteCodes;
    pStackTop : pByteCode;
  public
    Function Count : Integer;
    Function Peek(const index : integer) : pByteCode;
    procedure Push(const Item : TByteCode);
    function  Pop : TByteCode;
    procedure Init;
    procedure Finalize;
  end;


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

procedure TByteStack.Push(const Item: byte);
begin
  inc(FIndex);
  FItems.Add(Item);
  inc(pStackTop);
end;

{TByteCodeStack}

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
  FIndex := 0;
  FItems.Init;
  pStackTop := FItems.GetItem(0);
end;

function TByteCodeStack.Peek(const index: integer): pByteCode;
begin
  result := FItems.GetItem(Index);
end;

function TByteCodeStack.Pop: TByteCode;
var
 removed : PByteCode;
begin
  
  if FIndex = 0 then raise exception.create('Nothing to pop');

  if FItems.Remove = nil then raise exception.create('Failure to remove items from stack');
  Dec(FCount);
  dec(FIndex);
  dec(pStackTop);
  result := pStackTop^;
end;
           // 3 count compare to 1 count;
procedure TByteCodeStack.Push(const Item: TByteCode);
begin
  inc(FCount);
  inc(FIndex);
  FItems.Add(Item); //<==== the indexes get out of synch because the stack index and the array index are wildly different.
  inc(pStackTop);
end;




end.
