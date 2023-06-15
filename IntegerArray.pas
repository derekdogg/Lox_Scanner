unit IntegerArray;

interface


type

  TIntegers = record
  const
    NUM_SLOTS = 10;
    INCREMENT_CAPACITY_BY = 2;
  private
    FResizeCount   : integer;
    FIndex         : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pointer;
    procedure AllocateArray(var p : pointer; const size : integer);
    Procedure GrowArray;
    procedure GrowCapacity;
  public
    function SlotCount : integer;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : Integer) : integer;
    function  Item(const index : integer) : pInteger;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;

implementation

 

function TIntegers.SlotCount : integer;
begin
  result := FCapacity div Sizeof(Integer);
end;

function TIntegers.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TIntegers.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);

end;

function TIntegers.Count: integer;
begin
  result := FCount;
end;

procedure  TIntegers.GrowArray;
var
  pCopyItems : pointer;
begin
  pCopyItems := nil;
  if not isfull then exit;
  GrowCapacity;
  AllocateArray(pCopyItems,FCapacity);
  Move(FItems^, pCopyItems^, FPrevCapacity);  //copy existing memory into new memory;
  FreeMem(FItems);                           //free the old memory
  FItems := nil;                             //make the old memory nil
  FItems := pCopyItems;                      // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.
end;


function TIntegers.Add(const value : Integer) : integer;
var
  pIndex : pInteger;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TIntegers.Item(const index: integer): pInteger;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(Integer)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TIntegers.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TIntegers.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TIntegers.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(Integer))) div Sizeof(Integer);
end;

function TIntegers.IsFull : boolean;
begin
  result := (FCount * sizeof(Integer)) = FCapacity
end;

procedure TIntegers.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(Integer) = 0);
end;

Constructor TIntegers.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(Integer);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
