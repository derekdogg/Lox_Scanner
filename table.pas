unit Table;

interface

uses
  SysUtils,
  LOXTypes;


Const MAX_CAPACITY = 1000; //lets keep it reasonable - note this is not the size of the allocated array pNameValueItems.

type

  pNameValue = ^TNameValue;
  TNameValue = record
     name   : pValue;
     value  : pValue;
  end;



  pNameValueItems = ^TValuePairItems;
  TValuePairItems = array[0..MAX_CAPACITY - 1] of pNameValue;


  TValuePairs = record
  const
    // MAX_CAPACITY   = cMaxTokens;
    NUM_SLOTS      = 100;  //<== keep this small for now for testing how things get resized, and slot allocation etc.
    GROWTH_FACTOR  = 2;  //<== 4,8,16,32,64,128 etc
  private
    FResizeCount   : integer;
    FCount         : integer;
    FPrevcapacity  : integer;
    FCapacity      : integer;
    FItems         : pNameValueItems;
    procedure CopyItems(const old,new : pNameValueItems);
    procedure AllocateArray(var Items : pNameValueItems; const size : integer);
    procedure GrowArray;
    procedure growCapacity;
    function findNewIndex(const name  : string; const items : pNameValueItems): Integer;
    function getItem(const index: integer; const items : pNameValueItems): pNameValue;
    function DoFindEntry(const name   : string; const items : pNameValueItems): pNameValue;
    function  DoAdd(const value : pNameValue; const items : pNameValueItems) : boolean;
    procedure InsertItem(const index : integer; value : pNameValue; const Items : pNameValueItems);
    function FindNewKeyAndAddValue(const value : pNameValue; const items : pNameValueItems) : boolean;
  public
    function Find(const name  : string) : pNameValue;
    function ItemSize : integer;
    function InBounds(const index : integer; const capacity : Integer) : boolean;
    function SlotCount : integer;
    function Capacity : integer;
    function IsFull : boolean;
    function ResizeCount : integer;
    function FreeSlots : integer;
    function Count : integer;
    function Add(const value : pNameValue) : boolean;
    function NewValuePair(name : pValue; value : pValue) : pNameValue;
    constructor init;
    procedure finalize; //<-- no destructor allowed, seems weird.
 end;

 (*

 TValuePairIterator = record
 private
   FIndex : integer;
   FValuePairs : TValuePairs;
   FCurrent : pNameValue;
   FPrevious : pNameValue;
 public
   function Index : integer;
   function Current : pNameValue;
   function Previous : pNameValue;

   function Count : integer;
   function MoveFirst : pNameValue;
   function MoveNext  : pNameValue;
   function peekNext : pNameValue;
   function MovePrev  : pNameValue;
   function PeekPrev : pNameValue;
   function MoveLast  : pNameValue;
   procedure init(const Tokens : TValuePairs);
 end;  *)




implementation

function TValuePairs.NewValuePair(name : pValue; value : pValue) : pNameValue;
begin
  assert(assigned(Name),  'Name is nil');
  assert(assigned(value), 'value is nil');
  result := nil;
  if Find(Name.ToString) <> nil then exit; //already exists
  new(result);
  result.name := name;
  result.Value := Value;
  Add(result);
end;

function  TValuePairs.ItemSize : integer;
begin
  result := Sizeof(pNameValue);
end;


function TValuePairs.SlotCount : integer;
begin
  result := FCapacity div ItemSize;
end;

function TValuePairs.IsFull : boolean;
begin
  result := (FCount * ItemSize) = FCapacity
end;

function TValuePairs.Capacity : integer;
begin
  result := FCapacity;
end;


procedure TValuePairs.AllocateArray(var Items : pNameValueItems; const size : integer);
begin
  assert(Items = nil);
  getMem(Items,size);
  fillchar(Items^,size,#0);
end;

function TValuePairs.Count: integer;
begin
  result := FCount;
end;

procedure TValuePairs.CopyItems(const old,new : pNameValueItems);
var
  i : integer;
  str : pNameValue;

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

procedure  TValuePairs.GrowArray;
var
  pCopyItems : pNameValueItems;
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
function TValuePairs.Find(const name : string) : pNameValue;
begin
  result := DoFindEntry(name,FItems);
end;


function TValuePairs.DoFindEntry(const name : string; const items : pNameValueItems): pNameValue;
var
  index     : integer;
  prospect  : pNameValue;
  Hash      : Uint64;
  HashIndex : integer;

begin
  result := nil;

  Hash := GetHashString(name); //get the hash for debug before exit purposes; (i.e. to check it is a good hash).
  if FCount = 0 then exit; //empty so no clashes
  HashIndex :=  Hash and (slotcount -1);
  assert(inbounds(HashIndex,FCapacity),'index for Hash to seek exceeds dictionary limits');

  //forwards from ideal index
  index := HashIndex;
  if GetItem(index,items) = nil then exit; //we check the ideal index, if nothing in the slot, there can't be any matching strings

  //if we get here, then another string is in the ideal index, and the real string is maybe sitting somewhere else.
  while (Index < SlotCount) and (result = nil) do
  begin
    prospect := GetItem(index,items);

    if assigned(prospect) and (GetHashString(prospect.name.tostring) = hash) then
    begin
      result := prospect;
      exit;
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

      if assigned(prospect) and (GetHashString(prospect.name.tostring) = hash) then
      begin
        result := prospect;
        exit;
      end;
      inc(Index);
    end;
  end;

end;

(*function TValuePairs.FindEntry(const value : String): pNameValue;
var
  startIndex  : integer;
  index       : integer;
 // ValuePair   : pNameValue;
  hashstr     : pNameValue;
  prospect    : pNameValue;
begin
  result := nil;
  hashstr := NewValuePair(value);
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


function TValuePairs.FindNewIndex(const name  : string; const items : pNameValueItems): Integer;
var
  index     : integer;
  prospect  : pNameValue;
  HashIndex : integer;
  Hash      : Uint64;
begin
  assert(assigned(items),' items is nil');
  result := -1;
  Hash := GetHashString(name);
  HashIndex := Hash and (slotcount -1);
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

function TValuePairs.Add(const value : pNameValue) : boolean;
var
  pItem : pNameValue;
begin
  assert(assigned(Value),'value is nil');
  assert(assigned(Value.name), 'name is nil');
  assert(assigned(Value.value),'value is nil');

  pItem := nil;
  result := false;

  pItem := DoFindEntry(value.name.ToString,FItems);
  Assert(pItem = nil, 'Key violation, inserting the same Key into the hash table');

  if isfull then GrowArray;

  result := DoAdd(value,FItems);
end;

procedure TValuePairs.InsertItem(const index : integer; value : pNameValue; const Items : pNameValueItems);
begin
  assert(assigned(Value),'value is nil');
  assert(assigned(Value.name),'name is nil');
//  assert(assigned(Value.value),'value is nil');
  assert(InBounds(Index,FCapacity));
  Items[Index] := Value;
end;

function TValuePairs.FindNewKeyAndAddValue(const value : pNameValue; const items : pNameValueItems) : boolean;
var
  newIdx : integer;

begin
  assert(assigned(Value),'value is nil');
  assert(assigned(Value.name),'name is nil');
//  assert(assigned(Value.value),'value is nil');
  result := False;
  newIdx := FindNewIndex(Value.name.ToString,items);
  assert(newIdx <> -1,'new index is -1'); //this should not fail
  insertItem(NewIdx,value,Items);
  result := true;
end;

function TValuePairs.DoAdd(const value : pNameValue; const items : pNameValueItems) : boolean;
begin
  assert(assigned(Value),'value is nil');
  assert(assigned(Value.name),'name is nil');
//  assert(assigned(Value.value),'value is nil');
  result := FindNewKeyAndAddValue(value,Items);
  Assert(result = true, 'Could not find a new key for insertion');
  inc(FCount);
end;


function TValuePairs.InBounds(const index : integer; const capacity : Integer) : boolean;
begin
   result := Index * ItemSize <= capacity;
end;

function TValuePairs.getItem(const index: integer; const items : pNameValueItems): pNameValue;
begin
  assert(FCapacity > 0);
  assert(Items <> nil);
  assert(InBounds(index,FCapacity));
  result := items[Index];
//  inc(result,index); *)
end;

function TValuePairs.ResizeCount: integer;
begin
  result := FResizeCount;
end;

procedure TValuePairs.finalize;
var
  i : integer;
begin
  for i := 0 to SlotCount-1 do
  begin
    if FItems[i] <> nil then
    begin
      dispose(FItems[i]);
    end;
  end;
  if assigned(FItems) then
  begin
    freeMem(FItems);
    FItems := nil;
  end;
end;

function TValuePairs.FreeSlots: integer;
begin
  result := 0;
  if FCapacity = 0 then exit;
  result := (FCapacity - (FCount * ItemSize)) div ItemSize;
end;

procedure TValuePairs.growCapacity;
begin
  FPrevCapacity := FCapacity;
  FCapacity := FCapacity  * GROWTH_FACTOR;
  assert(FCapacity mod itemSize = 0);
  assert(FCapacity < MAX_CAPACITY,'Max size reached')
end;

Constructor TValuePairs.init;
begin
  FresizeCount := 0;
  FItems := nil;
  Fcount := 0;
  Fcapacity := NUM_SLOTS * sizeof(pNameValue);
  Fprevcapacity := Fcapacity;

  AllocateArray(FItems,Fcapacity);
end;


  (*function TValuePairIterator.Count : integer;
  begin
    result := FValuePairs.Count;
  end;

  function TValuePairIterator.MoveFirst : pNameValue;
  begin
    result := nil;
    if not FValuePairs.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FValuePairs.GetItem(FIndex);
    FPrevious := nil;
    result := FCurrent;
  end;

  function TValuePairIterator.MoveLast  : pNameValue;
  begin
    result := nil;
    if not FValuePairs.Count > 0 then exit;
    FIndex := FValuePairs.Count-1;
    FCurrent := FValuePairs.GetItem(FIndex);
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TValuePairIterator.MoveNext  : pNameValue;
  begin
    result := nil;
    if not FValuePairs.Count > 0 then exit;

    if FIndex = -1 then
    begin
      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FValuePairs.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FValuePairs.GetItem(FIndex);
      result := FCurrent;
    end;
  end;

  function TValuePairIterator.MovePrev  : pNameValue;
  begin
    result := nil;
    if not FValuePairs.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FValuePairs.GetItem(FIndex);
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;


  function TValuePairIterator.peekNext : pNameValue;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    inc(i);
    if i < FValuePairs.Count then
    begin
      result := FValuePairs.GetItem(i);
    end;
  end;

  function TValuePairIterator.PeekPrev : pNameValue;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    dec(i);
    if i >= 0 then
    begin
      result := FValuePairs.GetItem(i);
    end;
  end;


    function TValuePairIterator.Previous : pNameValue;
    begin
       result := FPrevious;
    end;

   function TValuePairIterator.Current : pNameValue;
   begin
     result := FCurrent;
   end;

    function TValuePairIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TValuePairIterator.init(const Tokens : TValuePairs);
  begin
    FValuePairs := Tokens;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end; *)

end.
