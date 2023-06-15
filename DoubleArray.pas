unit DoubleArray;

interface

type

  pSlotType = ^TSlotType;
  TSlotType = Double;

  TDoubles = record
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
    function Add(const value : TSlotType) : integer;
    function  Item(const index : integer) : pSlotType;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


implementation


 

function TDoubles.SlotCount : integer;
begin
  result := FCapacity div Sizeof(TSlotType);
end;

function TDoubles.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TDoubles.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);

end;

function TDoubles.Count: integer;
begin
  result := FCount;
end;

procedure  TDoubles.GrowArray;
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


function TDoubles.Add(const value : TSlotType) : integer;
var
  pIndex : pSlotType;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TDoubles.Item(const index: integer): pSlotType;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(TSlotType)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TDoubles.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TDoubles.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TDoubles.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(TSlotType))) div Sizeof(TSLotType);
end;

function TDoubles.IsFull : boolean;
begin
  result := (FCount * sizeof(TSlotType)) = FCapacity
end;

procedure TDoubles.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(TSlotType) = 0);
end;

Constructor TDoubles.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(TSlotType);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
