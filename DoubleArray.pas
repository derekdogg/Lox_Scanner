unit DoubleArray;

interface

type

  //pSlotType = ^TSlotType;
  //TSlotType = Double;

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
    function Add(const value : Double) : integer;
    function  Item(const index : integer) : pDouble;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


 TDoubleIterator = record
 private
   FIndex : integer;
   FDoubles : TDoubles;
   FCurrent : pDouble;
   FPrevious : pDouble;
 public
   function Index : integer;
   function Current : pDouble;
   function Previous : pDouble;

   function Count : integer;
   function MoveFirst : pDouble;
   function MoveNext  : pDouble;
   function peekNext : pDouble;
   function MovePrev  : pDouble;
   function PeekPrev : pDouble;
   function MoveLast  : pDouble;
   procedure init(const Doubles : TDoubles);
 end;



implementation

function TDoubleIterator.Count : integer;
  begin
    result := FDoubles.Count;
  end;

  function TDoubleIterator.MoveFirst : pDouble;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FDoubles.Item(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TDoubleIterator.MoveLast  : pDouble;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;
    FIndex := FDoubles.Count-1;
    FCurrent := FDoubles.Item(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TDoubleIterator.MoveNext  : pDouble;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;

    if FIndex = -1 then
    begin

      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FDoubles.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FDoubles.Item(FIndex);
      result := FCurrent;
    end;
  end;

  function TDoubleIterator.MovePrev  : pDouble;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FDoubles.Item(FIndex);
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;

  function TDoubleIterator.peekNext : pDouble;
  var
    i : integer;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;
    i := FIndex;
    inc(i);
    if i < FDoubles.Count-1 then
    begin
      result := FDoubles.Item(i);
    end;
  end;

  function TDoubleIterator.PeekPrev : pDouble;
  var
    i : integer;
  begin
    result := nil;
    if not FDoubles.Count > 0 then exit;
    i := FIndex;
    if i > 0 then
    begin
      dec(i);
      result := FDoubles.Item(i);
    end;
  end;


    function TDoubleIterator.Previous : pDouble;
    begin
       result := FPrevious;
    end;

   function TDoubleIterator.Current : pDouble;
   begin
     result := FCurrent;
   end;

    function TDoubleIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TDoubleIterator.init(const Doubles : TDoubles);
  begin
    FDoubles := Doubles;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end;



 

function TDoubles.SlotCount : integer;
begin
  result := FCapacity div Sizeof(Double);
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


function TDoubles.Add(const value : Double) : integer;
var
  pIndex : pDouble;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TDoubles.Item(const index: integer): pDouble;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(Double)) <= FCapacity);
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
  result := (FCapacity - (FCount * sizeof(Double))) div Sizeof(Double);
end;

function TDoubles.IsFull : boolean;
begin
  result := (FCount * sizeof(double)) = FCapacity
end;

procedure TDoubles.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(double) = 0);
end;

Constructor TDoubles.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(double);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
