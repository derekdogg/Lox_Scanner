unit ValueList;

interface
uses
 LoxTypes,values;

Const
  MAX_CAPACITY = 10000; //lets keep it reasonable - note this is not the size of the allocated array pNameValueItems.
  INCREMENT_CAPACITY_BY = 2;
  NUM_SLOTS = 8;
  GROWTH_FACTOR  = 2;
  
type

  pValues = ^TValues;

  TValues = array[0..MAX_CAPACITY - 1] of pValue;


  TValueList = record
  private
    FOwnValues     : Boolean;
    FResizeCount   : integer;
    FIndex         : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pValues;
    procedure AllocateArray(var Items : pValues; const size : integer);
    Procedure GrowArray;
    procedure GrowCapacity;
    function  ItemSize : integer;
    function InBounds(const index : integer; const capacity : Integer) : boolean;

  public
    function SlotCount : integer;
    function Remove(const index : integer) : PValue;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : pValue) : integer;
    function  GetItem(const index : integer) : pValue;
    procedure SetItem(const index : integer; const value : pValue);
    constructor init(Const OwnValues : Boolean);
    procedure finalize; //<-- no destructor allowed, seems weird.
    property Item[const Index : integer] : pValue read getItem write setItem;default;
 end;

 TValueStack = record
 private
    FCount    : Integer;
    FItems    : TValueList;
  public
    function GetItem(const index : integer) : pValue;
    procedure setItem(const index : integer; const value : pValue);
    Function Count : Integer;
    function Top : pValue;
    Function Peek : pValue; overload;
    Function Peek(const distance : integer) : pValue; overload;
    procedure Push(const Item : pValue);
    function  Pop : pValue;
    procedure Init;
    procedure Finalize;
    property Item[const index : integer] : pValue read getItem write setItem; default;
 end;




implementation

function TValueStack.Count: Integer;
begin
  result := FCount;
end;

procedure TValueStack.Finalize;
begin
  FItems.Finalize;
end;

procedure TValueStack.Init;
begin
  FCount := 0;
  FItems.Init(false);
end;

function TValueStack.GetItem(const index: integer): pValue;
begin
  result := FItems.item[index];
end;

function TValueStack.Peek(const distance: integer): pValue;
begin
  result := FItems.Item[FCount-1 -distance];
end;

function TValueStack.Peek: pValue;
begin
  result := Peek(0);
end;

function TValueStack.Pop: pValue;
var
  removed : pValue;
begin
  assert(FCount > 0,'Nothing to pop');
  result := Peek(0);
  removed :=FItems.Remove(FCount-1);

  dec(FCount);
end;

procedure TValueStack.Push(const Item: pValue);
begin
  inc(FCount);
  FItems.Add(Item);
end;

procedure TValueStack.setItem(const index: integer; const value: pValue);
begin
  FItems[index] := value; 
end;

function TValueStack.Top: pValue;
begin
  result := peek(0);
end;



{ TValueList }

function TValueList.Add(const value: pValue): integer;
begin
  assert(assigned(Value), 'Value being inserted is nil');
  growArray;
  FItems[FCount] := value;
  inc(FCount);
  result := FCount-1;
end;

function TValueList.Remove(const index : integer) : pValue;
begin
  assert(Index < FCount, 'removed index is > than count');
  assert(InBounds(Index,FCapacity),'index of removal outside bounds');
  result := FItems^[Index];
  assert(Result <> nil, 'removed index is not nil');
  Dec(FCount);
  Move(FItems^[Index + 1], FItems[Index],(FCount - Index) * SizeOf(PValue));
end;

procedure TValueList.AllocateArray(var Items : pValues; const size : integer);
begin
  assert(Items = nil);
  getMem(Items,size);
  fillchar(Items^,size,#0);
end;

function TValueList.Capacity: integer;
begin
  result := FCapacity;
end;

function TValueList.Count: integer;
begin
  result := FCount;
end;

procedure TValueList.finalize;
var
  i : integer;
  p : pValue;
begin
  if FOwnValues then
  begin
    for i := 0 to FCount-1 do
    begin
      p := Item[i];
      assert(p <> nil, 'finalize value item expected non nil value');
      DisposeValue(p);
    end;
  end;
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TValueList.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;

procedure TValueList.GrowArray;
var
  pCopyItems : pValues;
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

function  TValueList.ItemSize : integer;
begin
  result := Sizeof(pValue);
end;

procedure TValueList.GrowCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * GROWTH_FACTOR;
  assert(FCapacity mod itemSize = 0);
  assert(FCapacity < MAX_CAPACITY,'Max size reached')
end;

constructor TValueList.init(Const OwnValues : Boolean);
begin
  FOwnValues := OwnValues;
  FresizeCount := 0;
  FItems := nil;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(pValue);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,FCapacity);
end;

function TValueList.IsFull: boolean;
begin
  result := (FCount * ItemSize) = FCapacity;
end;

function TValueList.InBounds(const index : integer; const capacity : Integer) : boolean;
begin
   result := Index * ItemSize <= capacity;
end;

function TValueList.GetItem(const index: integer): pValue;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert(InBounds(index,FCapacity), 'Index out of bounds on value list Item(index)');
  result := FItems[index];
end;

function TValueList.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TValueList.SetItem(const index: integer; const value: pValue);
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

function TValueList.SlotCount: integer;
begin
  result := FCapacity div ItemSize;
end;

end.
