unit Chunk;

interface
uses Loxtypes;


(* Taken from crafting interpreters pp398 grow array - this hopefully mimics the c code close enough
    1.Allocate a new array with more capacity.
    2.Copy the existing elements from the old array to the new one.
    3.Store the new capacity.
    4.Delete the old array.
    5.Update code to point to the new array.
    6.Store the element in the new array now that there is room.
    7.Update the count.
*)
(*Note : obviously, you don't need to create your own dynamic arrays in delphi, but it's kind of fun to do it yourself.
  Haha. Yeah, right.
*)

const
   MIN_CAPACITY = 1;
   INCREMENT_CAPACITY_BY = 2;

type

 pSlotType = ^TSlotType;
 TSlotType = integer;

 TDynamicArray = record
    index         : integer;
    count         : integer;
    prevcapacity  : integer;
    capacity      : integer;
    pItems        : pointer;
    function IsFull : boolean;
    function  Add(const value : TSlotType) : integer;
    function  Item(const index : integer) : pSlotType;
    procedure AllocateArray(var p : pointer; const size : integer);
    procedure init;
    procedure finalize;
    Procedure GrowArray;
    procedure GrowCapacity;
 end;


 TChunk = record
   OPCodes      : TDynamicArray;
   ConstantPool : TDynamicArray;
   procedure init;
   procedure finalize;
 end;


implementation


procedure TDynamicArray.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);
end;

procedure  TDynamicArray.GrowArray;
var
  pCopyItems : pointer;
begin
  pCopyItems := nil;
  if not isfull then exit;
  GrowCapacity;
  AllocateArray(pCopyItems,Capacity);
  Move(pItems^, pCopyItems^, prevcapacity);  //copy existing memory into new memory;
  FreeMem(pItems);                           //free the old memory
  pItems := nil;                             //make the old memory nil
  pItems := pCopyItems;                      // set the old memory to the new memory;
end;


function TDynamicArray.Add(const value : TSlotType) : integer;
var
  pIndex : pSlotType;
begin
  result := -1;
  GrowArray;
  pIndex  := Item(Count);
  pIndex^ := value;
  inc(Count);
  result := Count-1;
end;

function TDynamicArray.Item(const index: integer): pSlotType;
begin
  assert(capacity > 0);
  assert(pItems <> nil);
  assert((index * sizeof(pSlotType)) <= capacity);
  result := nil;
  result := @pItems^;
  inc(result,index);
end;

procedure TDynamicArray.finalize;
begin
  if assigned(pItems) then
  begin
    freeMem(pItems);
    pItems := nil;
  end;
end;

function TDynamicArray.IsFull : boolean;
begin
  result := (count * sizeof(TSlotType)) = capacity
end;

procedure TDynamicArray.growCapacity;
begin
  prevCapacity := capacity;
  capacity := capacity  * INCREMENT_CAPACITY_BY;
end;

procedure TDynamicArray.init;
begin
  pItems := nil;
  index := 0;
  count := 0;
  capacity := MIN_CAPACITY * sizeof(TSlotType);
  prevcapacity := capacity;
  AllocateArray(pItems,capacity);
end;

{ TChunk }
procedure TChunk.finalize;
begin
  OPCodes.Finalize;
  ConstantPool.Finalize;
end;

procedure TChunk.init;
begin
   OPCodes.Init;
   ConstantPool.Init;
end;

end.
