unit ByteCodesArray;

interface

uses
  sysutils,LOXTypes;

type

  TByteCodes = record
  const
   // MAX_CAPACITY   = cMaxTokens;
    NUM_SLOTS      = 1000;
    GROWTH_FACTOR  = 2;
  private
    FResizeCount   : integer;
    FIndex         : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pointer;
    procedure AllocateArray(var p : pointer; const size : integer);
    procedure GrowArray(const Amount : integer);
    procedure growCapacity(const Amount : integer);

  public
    function ItemSize : integer;
    function InBounds(const Index : integer) : boolean;
    function Index : integer;
    function SlotCount : integer;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : TByteCode) : integer;
    function Remove : pByteCode;
    function  GetItem(const index : integer) : pByteCode;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


 TByteCodeIterator = record
 private
   FIndex : integer;
   FTokens : TByteCodes;
   FCurrent : pByteCode;
   FPrevious : pByteCode;
 public
   function Index : integer;
   function Current : pByteCode;
   function Previous : pByteCode;

   function Count : integer;
   function MoveFirst : pByteCode;
   function MoveNext  : pByteCode;
   function peekNext : pByteCode;
   function MovePrev  : pByteCode;
   function PeekPrev : pByteCode;
   function MoveLast  : pByteCode;
   procedure init(const Tokens : TByteCodes);
 end;

implementation

function  TByteCodes.ItemSize : integer;
begin
  result := Sizeof(TByteCode);
end;

function TByteCodes.Index : integer;
begin
  result := FIndex;
end;

function TByteCodes.SlotCount : integer;
begin
  result := FCapacity div ItemSize;
end;

function TByteCodes.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TByteCodes.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);
end;

function TByteCodes.Count: integer;
begin
  result := FCount;
end;

procedure  TByteCodes.GrowArray(const Amount : integer);
var
  pCopyItems : pointer;
begin
  Assert(Amount > 0);
  pCopyItems := nil;
  //if not isfull then exit;
  GrowCapacity(Amount);
  AllocateArray(pCopyItems,FCapacity);
  Move(FItems^, pCopyItems^, FPrevCapacity);  //copy existing memory into new memory;
  FreeMem(FItems);                           //free the old memory
  FItems := nil;                             //make the old memory nil
  FItems := pCopyItems;                      // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.
end;


function TByteCodes.Remove : pByteCode;
begin
  result := nil;
  if FCount = 0 then exit;
  result := getItem(FCount);
  dec(FIndex);
  dec(FCount);
end;


function TByteCodes.Add(const value : TByteCode) : integer;
var
  pIndex : pByteCode;
begin
  result := FCount;
  if isfull then GrowArray(GROWTH_FACTOR);
  pIndex  := getItem(FCount);
  pIndex^ := value;
  inc(FIndex);
  inc(FCount);
end;


function TByteCodes.InBounds(const Index : integer) : boolean;
begin
   result := Index * ItemSize <= FCapacity;
end;
 
function TByteCodes.getItem(const index: integer): pByteCode;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert(InBounds(Index));
  result := @FItems^;
  inc(result,index);
end;

function TByteCodes.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TByteCodes.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TByteCodes.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;

function TByteCodes.IsFull : boolean;
begin
  result := (FCount * ItemSize) = FCapacity
end;

procedure TByteCodes.growCapacity(const Amount : integer);
begin
  //if Amount + FCapacity > MAX_CAPACITY then raise exception.create('max token size reached');
  Assert(Amount > 0);
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * Amount; //INCREMENT_CAPACITY_BY;
  assert(FCapacity mod itemSize = 0);
end;

Constructor TByteCodes.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(TByteCode);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


  function TByteCodeIterator.Count : integer;
  begin
    result := FTokens.Count;
  end;

  function TByteCodeIterator.MoveFirst : pByteCode;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FTokens.GetItem(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TByteCodeIterator.MoveLast  : pByteCode;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := FTokens.Count-1;
    FCurrent := FTokens.GetItem(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TByteCodeIterator.MoveNext  : pByteCode;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;

    if FIndex = -1 then
    begin

      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FTokens.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FTokens.GetItem(FIndex);
      result := FCurrent;
    end;
  end;

  function TByteCodeIterator.MovePrev  : pByteCode;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FTokens.GetItem(FIndex);
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;

  function TByteCodeIterator.peekNext : pByteCode;
  var
    i : integer;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    i := FIndex;
    inc(i);
    if i < FTokens.Count-1 then
    begin
      result := FTokens.GetItem(i);
    end;
  end;

  function TByteCodeIterator.PeekPrev : pByteCode;
  var
    i : integer;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    i := FIndex;
    if i > 0 then
    begin
      dec(i);
      result := FTokens.GetItem(i);
    end;
  end;


    function TByteCodeIterator.Previous : pByteCode;
    begin
       result := FPrevious;
    end;

   function TByteCodeIterator.Current : pByteCode;
   begin
     result := FCurrent;
   end;

    function TByteCodeIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TByteCodeIterator.init(const Tokens : TByteCodes);
  begin
    FTokens := Tokens;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end;

end.
