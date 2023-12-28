unit HashTable;

interface

(*re-write the existing "table.pas" code

  We're gonna play around with different implementations to determine the optimal
  speed - which is important as the hashtable is potentially accessed multiple times
  especially in recursive sub-routines accessing globals.

  The old one is a) too complicated, b) probably too slow. c) buggy.

  we will find out.

  It's a relatively simple problem
  1) Get an index of a string based on a hash of the string and the size of the array.
  2) set the optimal index
  3) if the optimal index is not set, start searching for other optimal indexes


  Considerations

  growing an existing array. You will need to copy the existing items and rehash
  all the indexes. This is timeconsuming.

  The way the current one works is that it copies itself internally to a new array

  an interesting idea is to create a new array and spawn itself...???


  Steps

  1 - just create a dynamic array 1st. Really do we even need a dictionary at this point?

  I mean is it really going to be faster to scan say 128 items of string, compared to dictionary?


  Establish that fact, 1st.



*)

uses
  values;

type

   TNameValue = record
     Flag  : boolean; //whether in use or not.
     Name  : String;
     Value : TValueRecord;
   end;

   TNameValues = array of TNameValue;

   TGlobals = record
   const Global_Capacity = 256;
   private
     FCapacity : integer;
     FCount : integer;

     procedure IncreaseCapacity;
   public
     function Find(const name : String; var OutValue : TNameValue) : boolean;
     function Count : integer;
     procedure Add(const Value : TNameValue);
     function Get(const index : integer) : TNameValue;
     function IndexOf(const nameValue : TNameValue) : Integer;
     procedure setValue(const index: integer; const Namevalue: TNameValue);
     Procedure Init;

   end;

var
  FGlobals : TGlobals;
  FItems : TNameValues;

implementation

{ TGlobals }

procedure TGlobals.Add(const Value: TNameValue);
begin
  assert(IndexOf(value) = -1, 'record already exists in the table');

  if FCount = FCapacity then
    IncreaseCapacity;

  FItems[FCount] := Value;

  inc(FCount);
end;

function TGlobals.Count: integer;
begin
  result := FCount;
end;

procedure TGlobals.init;
begin
  FCapacity := Global_Capacity;
  SetLength(FItems,FCapacity);
  FCount := 0;
end;


function TGlobals.Get(const index: integer): TNameValue;
begin
  assert(index >= 0, 'Index is < 0');
  assert(index < FCapacity, 'Index is > Capacity');
  result := FItems[Index];
end;

procedure TGlobals.IncreaseCapacity;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems,FCapacity);
end;

function TGlobals.IndexOf(const NameValue : TNameValue): Integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to FCount - 1 do
  begin
    if FItems[i].Name = NameValue.Name then
    begin
      result := i;
      break;
    end;
  end;
end;

function TGlobals.Find(const name : String; var OutValue : TNameValue) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to FCount - 1 do
  begin
    if FItems[i].Name = Name then
    begin
      outValue := FItems[i];
      result := true;
      break;
    end;
  end;

end;

procedure TGlobals.setValue(const index: integer; const Namevalue: TNameValue);
begin
  assert(index >= 0, 'Index is < 0');
  assert(index < FCapacity, 'Index is > Capacity');
  FItems[Index].Value := NameValue.Value;
end;




initialization
  FGlobals.Init;

finalization

end.
