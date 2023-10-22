unit ByteArray;

interface

uses LoxTypes;

Const
  MAX_CAPACITY = 1000; //lets keep it reasonable - note this is not the size of the allocated array pNameValueItems.
  INCREMENT_CAPACITY_BY = 2;
  NUM_SLOTS = 100;
  GROWTH_FACTOR  = 2;
type

  pbytes = ^TbyteList;

  TByteList = array[0..MAX_CAPACITY - 1] of pByte;


  TDynamicBytes = record
  private
    FOwnbytes      : Boolean;
    FResizeCount   : integer;
    FIndex         : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pbytes;
    procedure AllocateArray(var Items : pbytes; const size : integer);
    Procedure GrowArray;
    procedure GrowCapacity;
    function  ItemSize : integer;
    function InBounds(const index : integer; const capacity : Integer) : boolean;

  public
    function SlotCount : integer;
    function Remove(const index : integer) : pByte;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : pByte) : integer;
    function  GetItem(const index : integer) : pByte;
    procedure SetItem(const index : integer; const value : pByte);
    constructor init(Const Ownbytes : Boolean);
    procedure finalize; //<-- no destructor allowed, seems weird.
    property Item[const Index : integer] : pByte read getItem write setItem;default;
 end;

 TDynamicBytestack = record
 private
    FCount    : Integer;
    FItems    : TDynamicBytes;
  public
    function GetItem(const index : integer) : pByte;
    procedure setItem(const index : integer; const value : pByte);
    Function Count : Integer;
    function Top : pByte;
    Function Peek : pByte; overload;
    Function Peek(const distance : integer) : pByte; overload;
    procedure Push(const Item : pByte);
    function  Pop : pByte;
    procedure Init;
    procedure Finalize;
    property Item[const index : integer] : pByte read getItem write setItem; default;
 end;




implementation

function TDynamicBytestack.Count: Integer;
begin
  result := FCount;
end;

procedure TDynamicBytestack.Finalize;
begin
  FItems.Finalize;
end;

procedure TDynamicBytestack.Init;
begin
  FCount := 0;
  FItems.Init(false);
end;

function TDynamicBytestack.GetItem(const index: integer): pByte;
begin
  result := FItems.item[index];
end;

function TDynamicBytestack.Peek(const distance: integer): pByte;
begin
  result := FItems.Item[FCount-1 -distance];
end;

function TDynamicBytestack.Peek: pByte;
begin
  result := Peek(0);
end;

function TDynamicBytestack.Pop: pByte;
var
  removed : pByte;
begin
  assert(FCount > 0,'Nothing to pop');
  result := Peek(0);
  removed :=FItems.Remove(FCount-1);

  dec(FCount);
end;

procedure TDynamicBytestack.Push(const Item: pByte);
begin
  inc(FCount);
  FItems.Add(Item);
end;

procedure TDynamicBytestack.setItem(const index: integer; const value: pByte);
begin
  FItems[index] := value; 
end;

function TDynamicBytestack.Top: pByte;
begin
  result := peek(0);
end;



{ TDynamicBytes }

function TDynamicBytes.Add(const value: pByte): integer;
begin
  assert(assigned(Value), 'Value being inserted is nil');
  growArray;
  FItems[FCount] := value;
  inc(FCount);
  result := FCount-1;
end;

function TDynamicBytes.Remove(const index : integer) : pByte;
begin
  assert(Index < FCount, 'removed index is > than count');
  assert(InBounds(Index,FCapacity),'index of removal outside bounds');
  result := FItems^[Index];
  assert(Result <> nil, 'removed index is not nil');
  Dec(FCount);
  Move(FItems^[Index + 1], FItems[Index],(FCount - Index) * SizeOf(pByte));
end;

procedure TDynamicBytes.AllocateArray(var Items : pbytes; const size : integer);
begin
  assert(Items = nil);
  getMem(Items,size);
  fillchar(Items^,size,#0);
end;

function TDynamicBytes.Capacity: integer;
begin
  result := FCapacity;
end;

function TDynamicBytes.Count: integer;
begin
  result := FCount;
end;

procedure TDynamicBytes.finalize;
var
  i : integer;
  p : pByte;
begin
  if FOwnbytes then
  begin
    for i := 0 to FCount-1 do
    begin
      p := Item[i];
      assert(p <> nil, 'finalize value item expected non nil value');
      Dispose(p);
    end;
  end;
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TDynamicBytes.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;

procedure TDynamicBytes.GrowArray;
var
  pCopyItems : pbytes;
begin
  if not isfull then exit;
  pCopyItems := nil;


  GrowCapacity;
  AllocateArray(pCopyItems,FCapacity);

  Move(FItems^, pCopyItems^, FPrevCapacity);

  FreeMem(FItems);                           //free the old memory
  FItems := nil;                             //make the old memory nil
  FItems := pCopyItems;                      // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.

end;

function  TDynamicBytes.ItemSize : integer;
begin
  result := Sizeof(pByte);
end;

procedure TDynamicBytes.GrowCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * GROWTH_FACTOR;
  assert(FCapacity mod itemSize = 0);
  assert(FCapacity < MAX_CAPACITY,'Max size reached')
end;

constructor TDynamicBytes.init(Const Ownbytes : Boolean);
begin
  FOwnbytes := Ownbytes;
  FresizeCount := 0;
  FItems := nil;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(pByte);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,FCapacity);
end;

function TDynamicBytes.IsFull: boolean;
begin
  result := (FCount * ItemSize) = FCapacity;
end;

function TDynamicBytes.InBounds(const index : integer; const capacity : Integer) : boolean;
begin
   result := Index * ItemSize <= capacity;
end;

function TDynamicBytes.GetItem(const index: integer): pByte;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert(InBounds(index,FCapacity), 'Index out of bounds on value list Item(index)');
  result := FItems[index];
end;

function TDynamicBytes.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TDynamicBytes.SetItem(const index: integer; const value: pByte);
begin
  if FItems[index] = value then exit;
  if FItems[Index] = nil then
  begin
    FItems[index] := value;
    exit;
  end;
  dispose(FItems[index]);
  FItems[index] := value;
end;

function TDynamicBytes.SlotCount: integer;
begin
  result := FCapacity div ItemSize;
end;

end.
