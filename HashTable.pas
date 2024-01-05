unit HashTable;

interface
 (* at the moment this is a pseudo hash table, in that it doesn't hash the value to obtain the index:)

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
     FItems : TNameValues;
     FCapacity : integer;
     FCount : integer;

     procedure IncreaseCapacity;
   public
     function Find(const name : String; var OutValue : TNameValue) : boolean; inline;
     function Count : integer; inline;
     procedure Add(const Value : TNameValue); inline;
     function Get(const index : integer) : TNameValue;inline;

     function IndexOf(const nameValue : TNameValue) : Integer; inline;
     procedure setValue(const index: integer; const Namevalue: TNameValue); inline;
     Procedure Init;

   end;


  //FItems : TNameValues;

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


finalization

end.




