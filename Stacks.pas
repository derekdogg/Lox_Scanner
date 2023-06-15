unit Stacks;

interface
uses
  IntegerArray,
  ByteArray,
  DoubleArray;


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

  TDoubleStack = record
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
  FItems.Init;
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

{ TDoubleStack }

procedure TDoubleStack.Finalize;
begin
  FItems.Finalize;
end;

procedure TDoubleStack.Init;
begin
  FIndex := 0;
  FItems.Init;
  pStackTop := FItems.Item(0);
end;

function TDoubleStack.Pop: Byte;
begin
  if FIndex = 0 then raise exception.create('Nothing to pop');
  dec(FIndex);
  dec(pStackTop);
  result := pStackTop^;
end;

procedure TDoubleStack.Push(const Item: byte);
begin
   inc(FIndex);
  FItems.Add(Item);
  inc(pStackTop);
end;

end.
