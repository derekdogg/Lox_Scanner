unit Table;

interface

uses
  SysUtils,
  LOXTypes;


Const MAX_CAPACITY = 1000; //lets keep it reasonable - note this is not the size of the allocated array PLoxStringItems.

type

  PLoxStringItems = ^TLoxStringItems;

  TLoxStringItems = array[0..MAX_CAPACITY - 1] of PloxString;


  TLoxStrings = record
  const
   // MAX_CAPACITY   = cMaxTokens;
    NUM_SLOTS      = 4;  //<== keep this small for now for testing how things get resized, and slot allocation etc.
    GROWTH_FACTOR  = 2;  //<== 4,8,16,32,64,128 etc
  private
    FResizeCount   : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : PLoxStringItems;
    procedure CopyItems(const old,new : pLoxStringItems);
    procedure AllocateArray(var Items : PLoxStringItems; const size : integer);
    procedure GrowArray;
    procedure growCapacity;
    function findNewIndex(const loxString : pLoxString; const items : PLoxStringItems): Integer;
    function getItem(const index: integer; const items : PLoxStringItems): pLoxString;
    function DoFindEntry(const loxString : pLoxString; const items : PLoxStringItems): pLoxString;
    function  DoAdd(const value : ploxString; const items : PLoxStringItems) : integer;
    procedure InsertItem(const index : integer; value : pLoxString; const Items : PLoxStringItems);
    function FindNewKeyAndAddValue(const value : ploxString; const items : PLoxStringItems) : boolean;
  public
    function Find(const loxString : pLoxString) : pLoxString;
    function ItemSize : integer;
    function InBounds(const index : integer; const capacity : Integer) : boolean;
    function SlotCount : integer;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : ploxString) : integer;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;

 (*

 TloxStringIterator = record
 private
   FIndex : integer;
   FLoxStrings : TLoxStrings;
   FCurrent : pLoxString;
   FPrevious : pLoxString;
 public
   function Index : integer;
   function Current : pLoxString;
   function Previous : pLoxString;

   function Count : integer;
   function MoveFirst : pLoxString;
   function MoveNext  : pLoxString;
   function peekNext : pLoxString;
   function MovePrev  : pLoxString;
   function PeekPrev : pLoxString;
   function MoveLast  : pLoxString;
   procedure init(const Tokens : TLoxStrings);
 end;  *)



implementation

function  TLoxStrings.ItemSize : integer;
begin
  result := Sizeof(ploxString);
end;


function TLoxStrings.SlotCount : integer;
begin
  result := FCapacity div ItemSize;
end;

function TLoxStrings.IsFull : boolean;
begin
  result := (FCount * ItemSize) = FCapacity
end;

function TLoxStrings.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TLoxStrings.AllocateArray(var Items : PLoxStringItems; const size : integer);
begin
  assert(Items = nil);
  getMem(Items,size);
  fillchar(Items^,size,#0);
end;

function TLoxStrings.Count: integer;
begin
  result := FCount;
end;

procedure TLoxStrings.CopyItems(const old,new : pLoxStringItems);
var
  i : integer;
  str : pLoxString;

begin
  assert(assigned(new),'new items is nil');
  assert(assigned(old),'old items is nil');
   
  for i := 0 to FCount-1 do
  begin
    str := old[i];
    if str <> nil then
    begin
      Assert(FindNewKeyAndAddValue(str,new) = true, 'Could not find a new key for insertion into new table');
    end;
  end;
end;

procedure  TLoxStrings.GrowArray;
var
  pCopyItems : pLoxStringItems;
begin
  pCopyItems := nil;
  GrowCapacity;
  AllocateArray(pCopyItems,FCapacity);
  copyItems(FItems,pCopyItems);

//we can't just copy now, because the index of the hash involves the size of the array. So we basically just rehash and put them in the array , as per above.
//Move(FItems^, pCopyItems^, FPrevCapacity);  //copy existing memory into new memory;

  FreeMem(FItems);                           //free the old memory
  FItems := nil;                             //make the old memory nil
  FItems := pCopyItems;                      // set the old memory to the new memory;

  inc(FResizeCount); //<-- used for debug checking.
end;

// [0,1,2,3,4,5,6,7,8,9]
function TLoxStrings.Find(const loxString : pLoxString) : pLoxString;
begin
  result := DoFindEntry(loxString,FItems);
end;


function TLoxStrings.DoFindEntry(const loxString : pLoxString; const items : PLoxStringItems): pLoxString;
var
  index     : integer;
  prospect  : pLoxString;
  HashIndex : integer;

begin
  result := nil;
  if FCount = 0 then exit; //empty so no clashes
  HashIndex := LoxString.hash and (slotcount -1);
  assert(inbounds(HashIndex,FCapacity),'index for Hash to seek exceeds dictionary limits');

  //forwards from ideal index
  index := HashIndex;
  if GetItem(index,items) = nil then exit; //we check the ideal index, if nothing in the slot, there can't be any matching strings

  //if we get here, then another string is in the ideal index, and the real string is maybe sitting somewhere else.
  while (Index < SlotCount) and (result = nil) do
  begin
    prospect := GetItem(index,items);

    if assigned(prospect) and ((prospect.chars = LoxString.chars)) then
    begin
      result := prospect;
      //exit;
    end;
    inc(Index);
  end;

  //no finds moving forward, now try from beginning if possible to ideal index
  if (result = nil) and (HashIndex > 0) then
  begin
    Index := 0;
    while (Index < HashIndex) and (result = nil) do
    begin
      prospect := GetItem(index,items);

      if assigned(prospect) and ((prospect.chars = LoxString.chars)) then
      begin
        result := prospect;
        //exit;
      end;
      inc(Index);
    end;
  end;

end;

(*function TLoxStrings.FindEntry(const value : String): pLoxString;
var
  startIndex  : integer;
  index       : integer;
 // loxstring   : pLoxString;
  hashstr     : pLoxString;
  prospect    : pLoxString;
begin
  result := nil;
  hashstr := NewLoxString(value);
  index := hashstr.hash and (slotcount -1);
  startIndex := index;
  repeat
    assert(inbounds(Index),'index for Hash to seek exceeds dictionary limits');

    prospect := getItem(Index);

    if assigned(prospect) and ((prospect.chars = value)) then
    begin
      result := prospect;
      exit;
    end;

    inc(index);
    if index = SlotCount then index := 0;

  until (prospect = nil);   //<--- index = startindex -1 indicates that the loop went back to 0 then to the position before where we 1st started, and did not find the key
  dispose(hashStr);
end;  *)


function TLoxStrings.FindNewIndex(const loxString : pLoxString; const items : PLoxStringItems): Integer;
var
  index     : integer;
  prospect  : pLoxString;
  HashIndex : integer;

begin
  result := -1;
  HashIndex := LoxString.hash and (slotcount -1);
  assert(inbounds(HashIndex,FCapacity),'index for Hash to seek exceeds dictionary limits');

  //forwards from ideal index
  index := HashIndex;
  while (Index < SlotCount) and (result = -1) do
  begin
    prospect := GetItem(index,items);
    if prospect = nil then
    begin
      result := Index;
      //exit;
    end;
    inc(Index);
  end;

  //no finds moving forward, now try from beginning if possible to ideal index
  if (result = -1) and (HashIndex > 0) then
  begin
    Index := 0;
    while (Index < HashIndex) and (result = -1) do
    begin
      prospect := GetItem(index,items);

      if prospect = nil then
      begin
        result := index;
        //exit;
      end;
      inc(Index);
    end;
  end;
end;

function TLoxStrings.Add(const value : ploxString) : integer;
var
  pItem : pLoxString;
begin
  
  pItem := nil;
  result := -1;

  pItem := DoFindEntry(value,FItems);
  Assert(pItem = nil, 'Key violation');

  if isfull then GrowArray;

  result := DoAdd(value,FItems);
end;

procedure TLoxStrings.InsertItem(const index : integer; value : pLoxString; const Items : PLoxStringItems);
begin
  assert(InBounds(Index,FCapacity));
  Items[Index] := Value;
end;

function TLoxStrings.FindNewKeyAndAddValue(const value : ploxString; const items : PLoxStringItems) : boolean;
var
  newIdx : integer;

begin
  result := False;
  newIdx := FindNewIndex(Value,items);
  assert(newIdx <> -1,'new index is -1'); //this should not fail
  insertItem(NewIdx,value,Items);
  result := true;
end;

function TLoxStrings.DoAdd(const value : ploxString; const items : PLoxStringItems) : integer;
begin
  Assert(FindNewKeyAndAddValue(value,Items) = true, 'Could not find a new key for insertion');
  inc(FCount);
end;


function TLoxStrings.InBounds(const index : integer; const capacity : Integer) : boolean;
begin
   result := Index * ItemSize <= capacity;
end;

function TLoxStrings.getItem(const index: integer; const items : PLoxStringItems): pLoxString;
begin
  assert(FCapacity > 0);
  assert(Items <> nil);
  assert(InBounds(index,FCapacity));
  result := items[Index];
//  inc(result,index); *)
end;

function TLoxStrings.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TLoxStrings.finalize;
begin
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TLoxStrings.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;



procedure TLoxStrings.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * GROWTH_FACTOR;
  assert(FCapacity mod itemSize = 0);
  assert(FCapacity < MAX_CAPACITY,'Max size reached')
end;

Constructor TLoxStrings.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(pLoxString);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


  (*function TloxStringIterator.Count : integer;
  begin
    result := FLoxStrings.Count;
  end;

  function TloxStringIterator.MoveFirst : pLoxString;
  begin
    result := nil;
    if not FLoxStrings.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FLoxStrings.GetItem(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TloxStringIterator.MoveLast  : pLoxString;
  begin
    result := nil;
    if not FLoxStrings.Count > 0 then exit;
    FIndex := FLoxStrings.Count-1;
    FCurrent := FLoxStrings.GetItem(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TloxStringIterator.MoveNext  : pLoxString;
  begin
    result := nil;
    if not FLoxStrings.Count > 0 then exit;

    if FIndex = -1 then
    begin
      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FLoxStrings.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FLoxStrings.GetItem(FIndex);
      result := FCurrent;
    end;
  end;

  function TloxStringIterator.MovePrev  : pLoxString;
  begin
    result := nil;
    if not FLoxStrings.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FLoxStrings.GetItem(FIndex);
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;


  function TloxStringIterator.peekNext : pLoxString;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    inc(i);
    if i < FLoxStrings.Count then
    begin
      result := FLoxStrings.GetItem(i);
    end;
  end;

  function TloxStringIterator.PeekPrev : pLoxString;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    dec(i);
    if i >= 0 then
    begin
      result := FLoxStrings.GetItem(i);
    end;
  end;


    function TloxStringIterator.Previous : pLoxString;
    begin
       result := FPrevious;
    end;

   function TloxStringIterator.Current : pLoxString;
   begin
     result := FCurrent;
   end;

    function TloxStringIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TloxStringIterator.init(const Tokens : TLoxStrings);
  begin
    FLoxStrings := Tokens;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end; *)

end.
