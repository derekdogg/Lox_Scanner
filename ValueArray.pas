unit ValueArray;

interface

uses LOXTypes;

type

  //pSlotType = ^TSlotType;
  //TSlotType = TValue;

  TValues = record
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
    function Add(const value : TValue) : integer;
    function  Item(const index : integer) : pValue;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


 TValueIterator = record
 private
   FIndex : integer;
   FValues : TValues;
   FCurrent : pValue;
   FPrevious : pValue;
 public
   function Index : integer;
   function Current : pValue;
   function Previous : pValue;

   function Count : integer;
   function MoveFirst : pValue;
   function MoveNext  : pValue;
   function peekNext : pValue;
   function MovePrev  : pValue;
   function PeekPrev : pValue;
   function MoveLast  : pValue;
   procedure init(const TValues : TValues);
 end;



implementation

function TValueIterator.Count : integer;
  begin
    result := FValues.Count;
  end;

  function TValueIterator.MoveFirst : pValue;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FValues.Item(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TValueIterator.MoveLast  : pValue;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;
    FIndex := FValues.Count-1;
    FCurrent := FValues.Item(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TValueIterator.MoveNext  : pValue;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;

    if FIndex = -1 then
    begin

      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FValues.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FValues.Item(FIndex);
      result := FCurrent;
    end;
  end;

  function TValueIterator.MovePrev  : pValue;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FValues.Item(FIndex);
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;

  function TValueIterator.peekNext : pValue;
  var
    i : integer;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;
    i := FIndex;
    inc(i);
    if i < FValues.Count-1 then
    begin
      result := FValues.Item(i);
    end;
  end;

  function TValueIterator.PeekPrev : pValue;
  var
    i : integer;
  begin
    result := nil;
    if not FValues.Count > 0 then exit;
    i := FIndex;
    if i > 0 then
    begin
      dec(i);
      result := FValues.Item(i);
    end;
  end;


    function TValueIterator.Previous : pValue;
    begin
       result := FPrevious;
    end;

   function TValueIterator.Current : pValue;
   begin
     result := FCurrent;
   end;

    function TValueIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TValueIterator.init(const TValues : TValues);
  begin
    FValues := TValues;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end;



 

function TValues.SlotCount : integer;
begin
  result := FCapacity div Sizeof(TValue);
end;

function TValues.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TValues.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);

end;

function TValues.Count: integer;
begin
  result := FCount;
end;

procedure  TValues.GrowArray;
var
  pCopyItems : pointer;
begin
  pCopyItems := nil;
  if not isfull then exit;
  GrowCapacity;
  AllocateArray(pCopyItems,FCapacity);
  Move(FItems^, pCopyItems^, FPrevCapacity);  // copy existing memory into new memory;
  FreeMem(FItems);                            // free the old memory
  FItems := nil;                              // make the old memory nil
  FItems := pCopyItems;                       // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.
end;


function TValues.Add(const value : TValue) : integer;
var
  pIndex : pValue;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TValues.Item(const index: integer): pValue;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(TValue)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TValues.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TValues.finalize;
var
  Values : TValueIterator;
  Value  : pValue;
  i : integer;
begin
  Values.Init(Self);
  Value := Values.MoveLast;
  while Value <> nil do
  begin
    if value.Kind = lxObject then
    begin
      case Value.LoxObject.Kind of
        OBJ_STRING : begin
          dispose(pLoxString(Value.LoxObject));
        end;
      end;
    end;
    Value := Values.MovePrev;
  end;

  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TValues.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(TValue))) div Sizeof(TValue);
end;

function TValues.IsFull : boolean;
begin
  result := (FCount * sizeof(TValue)) = FCapacity
end;

procedure TValues.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(TValue) = 0);
end;

Constructor TValues.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(TValue);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
