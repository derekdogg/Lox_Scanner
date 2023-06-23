unit TokenArray;

interface

uses
  sysutils,LOXTypes;

type

  TTokens = record
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
    function Add(const value : TToken) : integer;
    function  GetItem(const index : integer) : pToken;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;


 TTokenIterator = record
 private
   FIndex : integer;
   FTokens : TTokens;
   FCurrent : pToken;
   FPrevious : pToken;
 public
   function Index : integer;
   function Current : pToken;
   function Previous : pToken;

   function Count : integer;
   function MoveFirst : pToken;
   function MoveNext  : pToken;
   function peekNext : pToken;
   function MovePrev  : pToken;
   function PeekPrev : pToken;
   function MoveLast  : pToken;
   procedure init(const Tokens : TTokens);
 end;



implementation

function  TTokens.ItemSize : integer;
begin
  result := Sizeof(TToken);
end;

function TTokens.Index : integer;
begin
  result := FIndex;
end;

function TTokens.SlotCount : integer;
begin
  result := FCapacity div ItemSize;
end;

function TTokens.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TTokens.AllocateArray(var p : pointer; const size : integer);
begin
  assert(p = nil);
  getMem(p,size);
  fillchar(p^,size,#0);
end;

function TTokens.Count: integer;
begin
  result := FCount;
end;

procedure  TTokens.GrowArray(const Amount : integer);
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


function TTokens.Add(const value : TToken) : integer;
var
  pIndex : pToken;
begin
  result := FCount;
  if isfull then GrowArray(GROWTH_FACTOR);
  pIndex  := getItem(FCount);
  pIndex^ := value;
  inc(FIndex);
  inc(FCount);
end;


function TTokens.InBounds(const Index : integer) : boolean;
begin
   result := Index * ItemSize <= FCapacity;
end;
 
function TTokens.getItem(const index: integer): pToken;
begin
  assert(FCapacity > 0);
  assert(FItems <> nil);
  assert(InBounds(FIndex));
  result := @FItems^;
  inc(result,index);
end;

function TTokens.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TTokens.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TTokens.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;

function TTokens.IsFull : boolean;
begin
  result := (FCount * ItemSize) = FCapacity
end;

procedure TTokens.growCapacity(const Amount : integer);
begin
  //if Amount + FCapacity > MAX_CAPACITY then raise exception.create('max token size reached');
  Assert(Amount > 0);
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * Amount; //INCREMENT_CAPACITY_BY;
  assert(FCapacity mod itemSize = 0);
end;

Constructor TTokens.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Findex := 0;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(TToken);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


  function TTokenIterator.Count : integer;
  begin
    result := FTokens.Count;
  end;

  function TTokenIterator.MoveFirst : pToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FTokens.GetItem(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TTokenIterator.MoveLast  : pToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := FTokens.Count-1;
    FCurrent := FTokens.GetItem(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TTokenIterator.MoveNext  : pToken;
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

  function TTokenIterator.MovePrev  : pToken;
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

  function TTokenIterator.peekNext : pToken;
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

  function TTokenIterator.PeekPrev : pToken;
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


    function TTokenIterator.Previous : pToken;
    begin
       result := FPrevious;
    end;

   function TTokenIterator.Current : pToken;
   begin
     result := FCurrent;
   end;

    function TTokenIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TTokenIterator.init(const Tokens : TTokens);
  begin
    FTokens := Tokens;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end;

end.
