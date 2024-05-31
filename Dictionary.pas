unit Dictionary;

interface

const
  Load_Factor = 0.75;
  
  Start_Dictionary_Size = 256;

type

  TKeyValue = record
    Name: string;
    Value: Pointer;
  end;

  TDictionaryRecord = record
    KeyValue: TKeyValue;
    Deleted: Boolean;
  end;

  TDictionaryRecords = array of TDictionaryRecord;

  TDictionary = class
  private
    FCount: Integer;
    FItems: TDictionaryRecords;
    procedure InitializeStorage;
    procedure GrowCapacity;
    function FindEmptySlot(Name: String): Integer;
    function IdealIndex(const Name: string): Integer;
    function GetHashString(const Value: string): UInt64;
    function getItem(const index: integer): TKeyValue;
  public
    constructor Create;
    function IndexOf(const KeyValue : TKeyValue) : integer;
    function Capacity: Integer;
    function Add(const KeyValue: TKeyValue): Integer;
    function Remove(const KeyValue: TKeyValue): Boolean;
    property Count: Integer read FCount;
    property Item[const index : integer] : TKeyValue read getItem; default; 
  end;

implementation

uses
  SysUtils, Dialogs;

procedure TDictionary.InitializeStorage;
begin
  FCount := 0;
  SetLength(FItems, Start_Dictionary_Size);
end;

procedure TDictionary.GrowCapacity;
var
  OldCapacity: Integer;
  OldItems: TDictionaryRecords;
  NewItems: TDictionaryRecords;
  i: Integer;
begin
  OldCapacity := Capacity;
  SetLength(NewItems, Capacity * 2);
  OldItems := FItems;
  FItems := NewItems;
  FCount := 0;

  for i := 0 to OldCapacity - 1 do
  begin
    if (OldItems[i].KeyValue.Name <> '') and not OldItems[i].Deleted then
    begin
      Add(OldItems[i].KeyValue);
    end;
  end;
end;

function TDictionary.IdealIndex(const Name: string): Integer;
begin
  Result := GetHashString(Name) mod Length(FItems);
end;

function TDictionary.GetHashString(const Value: string): UInt64;
const
  FNV_offset_basis: UInt64  = 2166136261;
  FNV_prime: UInt64  = 16777619;
var
  i: Integer;
begin
  Result := FNV_offset_basis;
  for i := 1 to Length(Value) do
  begin
    Result := UInt64(Result xor Byte(Value[i]));
    Result := Result * FNV_prime;
  end;
end;

function TDictionary.getItem(const index: integer): TKeyValue;
begin
  assert(index >= 0, 'Index for get item is not >= zero. TDictionary.getItem');
  assert(index <= Capacity-1, 'Index for get item is not <= Capacity. TDictionary.getItem');
  result := FItems[index].KeyValue;
end;

constructor TDictionary.Create;
begin
  InitializeStorage;
end;

function TDictionary.Capacity: Integer;
begin
  Result := Length(FItems);
end;


function TDictionary.Indexof(const KeyValue : TKeyValue): integer;
var
  startIdx, initialIndex: Integer;
begin
  Assert(KeyValue.Name > '', 'Key Value is empty. [TDictionary.Indexof]');

  Result := -1;

  if FCount = 0 then exit;

  initialIndex := IdealIndex(KeyValue.Name);
  startIdx := initialIndex;

  repeat
    if (FItems[startIdx].KeyValue.Name = KeyValue.Name) and not FItems[startIdx].Deleted then
    begin
       Result := startIdx;
       exit;
    end;

    startIdx := (startIdx + 1) mod Capacity;  // Wrap around using modulo
  until startIdx = initialIndex;
end;

function TDictionary.FindEmptySlot(Name: String): Integer;
var
  startIdx, initialIndex: Integer;

begin
  Assert(Name > '', 'Looking for empty slot with no name. [TDictionary.FindEmptySlot]');
  Result := -1;
  initialIndex := IdealIndex(Name);
  startIdx := initialIndex;

  repeat
    if (FItems[startIdx].KeyValue.Name = '') or (FItems[startIdx].Deleted) then
    begin
      Result := startIdx;
      exit;
    end;

    if (FItems[startIdx].KeyValue.Name = Name) and not FItems[startIdx].Deleted then
    begin
       exit;
    end;

    startIdx := (startIdx + 1) mod Capacity;  // Wrap around using modulo

  until startIdx = initialIndex;
end;

function TDictionary.Add(const KeyValue: TKeyValue): Integer;
begin
  Assert(KeyValue.Name > '', 'Key Value is empty. [TDictionary.Add]');

  if FCount >= Capacity * Load_Factor then
    GrowCapacity;

  Result := FindEmptySlot(KeyValue.Name);

  if Result <> -1 then
  begin
      FItems[Result].KeyValue := KeyValue;
      FItems[Result].Deleted := False;
      Inc(FCount);
  end;
end;

function TDictionary.Remove(const KeyValue: TKeyValue): Boolean;
var
  idx, initialIndex: Integer;
begin
  Assert(KeyValue.Name > '', 'Key Value is empty. [TDictionary.Remove]');
  Result := False;
  idx := IdealIndex(KeyValue.Name);
  initialIndex := idx;

  while FItems[idx].KeyValue.Name <> '' do
  begin
    if (FItems[idx].KeyValue.Name = KeyValue.Name) and not FItems[idx].Deleted then
    begin
      FItems[idx].Deleted := True;
      Dec(FCount);
      Result := True;
      Exit;
    end;

    idx := (idx + 1) mod Capacity;

    if idx = initialIndex then
      Exit;  // Key not found
  end;
end;

end.

