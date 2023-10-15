unit locals;

interface

uses
  LOXTypes;

Const
  MAX_LOCALS = 256;
  MAX_CAPACITY = 1000; //lets keep it reasonable - note this is not the size of the allocated array pNameValueItems.
  INCREMENT_CAPACITY_BY = 2;
  NUM_SLOTS = 10;
  GROWTH_FACTOR  = 2;

type

  pLocal = ^TLocal;
  TLocal = record
    Token : pToken;
    Depth : integer;
    IsCaptured : Boolean;
  end;
  pLocals = ^TLocals;
  TLocals = array[0..MAX_CAPACITY - 1] of pLocal;

  TLocalList = record
  private
    FOwnValues     : Boolean;
    FResizeCount   : integer;
    FIndex         : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pLocals;
    procedure AllocateArray(var Items : pLocals; const size : integer);
    Procedure GrowArray;
    procedure GrowCapacity;
    function  ItemSize : integer;
    function InBounds(const index : integer; const capacity : Integer) : boolean;
  public
    function Last : pLocal;
    function SlotCount : integer;
    function Remove(const index : integer) : pLocal;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : pLocal) : integer; overload;
    function add : pLocal; overload;
    function  GetItem(const index : integer) : pLocal;
    constructor init(Const OwnValues : Boolean);
    procedure finalize; //<-- no destructor allowed, seems weird.
    property item[const index : integer] : pLocal read Getitem; default;
 end;



implementation

function DisposeLocal(value : pLocal) : boolean;
begin
  assert(assigned(Value), ' value for disposal is nil');
  dispose(Value);
  value := nil;
end;


function TLocalList.add : pLocal;
begin
  new(result);
  fillchar(result^,sizeof(result^),#0);
  add(result);
end;

function TLocalList.Add(const value: pLocal): integer;
begin
  //assert(assigned(Value), 'Value being inserted is nil');
  assert(assigned(FItems),'Locals storage is nil');
  FItems[FCount] := value;
  inc(FCount);
  result := FCount-1;
end;


(*
1 2 3 4 5 6 7 8 9 0
    *


*)
function TLocalList.Remove(const index : integer) : pLocal;

begin
  assert(InBounds(Index,FCapacity),'index of removal outside bounds');
  assert(Index < FCount, 'removed index is > than count');
  result := FItems^[Index];
  assert(Result <> nil, 'removed index is not nil');
  dec(FCount);
  Move(FItems^[Index + 1], FItems[Index], (FCount - Index) * SizeOf(pLocal));
end;

procedure TLocalList.AllocateArray(var Items : pLocals; const size : integer);
begin
  assert(Items = nil);
  getMem(Items,size);
  fillchar(Items^,size,#0);
end;

function TLocalList.Capacity: integer;
begin
  result := FCapacity;
end;

function TLocalList.Count: integer;
begin
  result := FCount;
end;

procedure TLocalList.finalize;
var
  i : integer;
  p : pLocal;
begin
  if FOwnValues then
  begin
    for i := 0 to FCount-1 do
    begin
      p := GetItem(i);

      if p <> nil then
      //  assert(p <> nil, 'finalize value item expected non nil value');
        DisposeLocal(p);
    end;
  end;
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TLocalList.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;


procedure TLocalList.GrowArray;
var
  pCopyItems : pLocals;
begin
  pCopyItems := nil;
  GrowCapacity;
  AllocateArray(pCopyItems,FCapacity);

  Move(FItems^, pCopyItems^, FPrevCapacity);

  FreeMem(FItems);                           //free the old memory
  FItems := nil;                             //make the old memory nil
  FItems := pCopyItems;                      // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.

end;

function  TLocalList.ItemSize : integer;
begin
  result := Sizeof(pLocal);
end;

function TLocalList.Last: pLocal;
begin
  result := GetItem(FCount-1);
end;

procedure TLocalList.GrowCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * GROWTH_FACTOR;
  assert(FCapacity mod itemSize = 0);
  assert(FCapacity < MAX_CAPACITY,'Max size reached')
end;

constructor TLocalList.init(Const OwnValues : Boolean);
begin
  FOwnValues := OwnValues;
  FresizeCount := 0;
  FItems := nil;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(pLocal);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,FCapacity);
end;

function TLocalList.IsFull: boolean;
begin
  result := (FCount * ItemSize) = FCapacity;
end;

function TLocalList.InBounds(const index : integer; const capacity : Integer) : boolean;
begin
   result := Index * ItemSize <= capacity;
end;

function TLocalList.GetItem(const index: integer): pLocal;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert(InBounds(index,FCapacity), 'index out of bounds for locals');
  result := FItems[index];
end;

function TLocalList.ResizeCount: integer;
begin
  result := FResizeCount;
end;

function TLocalList.SlotCount: integer;
begin
  result := FCapacity div ItemSize;
end;



end.
