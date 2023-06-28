unit ByteArray;

interface

type



  TBytes = record
  const
    //NUM_SLOTS = 10;
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
    function Index : integer;
    function SlotCount : integer;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : byte) : integer;
    function  Item(const index : integer) : pByte;
    constructor init(const Count : integer);
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


implementation


function TBytes.Index : integer;
begin
  result := FIndex;
end;

function TBytes.SlotCount : integer;
begin
  result := FCapacity div Sizeof(Byte);
end;

function TBytes.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TBytes.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);

end;

function TBytes.Count: integer;
begin
  result := FCount;
end;

procedure  TBytes.GrowArray;
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


function TBytes.Add(const value : Byte) : integer;
var
  pIndex : pByte;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FIndex);
  inc(FCount);
end;

function TBytes.Item(const index: integer): pByte;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(Byte)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TBytes.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TBytes.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TBytes.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(Byte))) div Sizeof(Byte);
end;

function TBytes.IsFull : boolean;
begin
  result := (FCount * sizeof(Byte)) = FCapacity
end;

procedure TBytes.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(Byte) = 0);
end;

Constructor TBytes.init(const Count : integer);
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := count * sizeof(Byte);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
