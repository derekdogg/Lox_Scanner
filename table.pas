unit table;

interface

uses
  LOXTypes;

type

  pEntry = ^TEntry;
  TEntry = record
    key : pLoxString;
    val : pValue;
  end;

  TEntries = record
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
    function Add(const value : TEntry) : integer;
    function Item(const index : integer) : pEntry;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


(*

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry* entries;
} Table;
 *)

implementation





function TEntries.SlotCount : integer;
begin
  result := FCapacity div Sizeof(TEntry);
end;

function TEntries.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TEntries.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);
end;

function TEntries.Count: integer;
begin
  result := FCount;
end;

procedure  TEntries.GrowArray;
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


function TEntries.Add(const value : TEntry) : integer;
var
  pIndex : pEntry;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TEntries.Item(const index: integer): pEntry;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(TEntry)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TEntries.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TEntries.finalize;
var
 // Values : TEntryIterator;
  Value  : pEntry;
  i : integer;
begin
  (*Values.Init(Self);
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
  end; *)

  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TEntries.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(TEntry))) div Sizeof(TEntry);
end;

function TEntries.IsFull : boolean;
begin
  result := (FCount * sizeof(TEntry)) = FCapacity
end;

procedure TEntries.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(TEntry) = 0);
end;

Constructor TEntries.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(TEntry);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


end.
