unit Chunk;

interface
uses Loxtypes;


(* Taken from crafting interpreters pp398 grow array - this hopefully mimics the c code close enough *)


type

  pSlotType = ^TSlotType;
  TSlotType = integer;

  TDynamicArray = record
  const
    MIN_CAPACITY = 2;
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
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSpace : integer;
    function Count : integer;
    function Add(const value : TSlotType) : integer;
    function  Item(const index : integer) : pSlotType;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


 TChunk = record
   OPCodes      : TDynamicArray;
   ConstantPool : TDynamicArray;
   procedure init;
   procedure finalize;
 end;


implementation


function TDynamicArray.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TDynamicArray.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);

end;

function TDynamicArray.Count: integer;
begin
  result := FCount;
end;

procedure  TDynamicArray.GrowArray;
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


function TDynamicArray.Add(const value : TSlotType) : integer;
var
  pIndex : pSlotType;
begin
  result := FCount;
  GrowArray;
  pIndex  := Item(FCount);
  pIndex^ := value;
  inc(FCount);
end;

function TDynamicArray.Item(const index: integer): pSlotType;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert((FIndex * sizeof(TSlotType)) <= FCapacity);
  result := @FItems^;
  inc(result,index);
end;

function TDynamicArray.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TDynamicArray.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TDynamicArray.FreeSpace: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * sizeof(TSlotType))) div Sizeof(TSLotType);
end;

function TDynamicArray.IsFull : boolean;
begin
  result := (FCount * sizeof(TSlotType)) = FCapacity
end;

procedure TDynamicArray.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * INCREMENT_CAPACITY_BY;
  assert(FCapacity mod sizeof(TSlotType) = 0);
end;

Constructor TDynamicArray.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := MIN_CAPACITY * sizeof(TSlotType);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
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
