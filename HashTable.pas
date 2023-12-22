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


   TGlobals = class
   const Global_Capacity = 256;
   private
     FCapacity : integer;
     FCount : integer;
     FItems : TNameValues;
   protected
     procedure IncreaseCapacity;
   public
     function Count : integer;
     procedure Add(const Value : TNameValue);
     function Get(const index : integer) : TNameValue;
     function IndexOf(const nameValue : TNameValue) : Integer;
     procedure setValue(const index: integer; const Namevalue: TNameValue);
     constructor create;
     destructor destroy; override;
   end;


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

constructor TGlobals.create;
begin
  FCapacity := Global_Capacity;
  SetLength(FItems,FCapacity);
  FCount := 0;
end;

destructor TGlobals.destroy;
begin

  inherited;
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
      exit;
    end;
  end;
end;

procedure TGlobals.setValue(const index: integer; const Namevalue: TNameValue);
begin
  assert(index >= 0, 'Index is < 0');
  assert(index < FCapacity, 'Index is > Capacity');
  FItems[Index].Value := NameValue.Value;
end;

end.
