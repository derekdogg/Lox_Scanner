unit table;

interface
uses
  LOXTypes;

type

  pEntry = ^TEntry;
  TEntry = record
    key : pLoxString;
    val : pValue;
  end;

  TTable = record
    count : integer;
    capacity : integer;
    Entries : pEntry;
  end;


(*

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry* entries;
} Table;
 *)

implementation

end.
