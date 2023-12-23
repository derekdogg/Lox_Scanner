unit locals;

interface

uses
  classes,LOXTypes;

type


  TLocal = record
    Name  : String;

    Token : TToken;
    Depth : integer;
    IsCaptured : Boolean;
  end;


  TLocalList = array of TLocal;

  TLocals = class
  const Local_Capacity = 256;
  private
    FItems : TLocalList;
    FCapacity  : integer;
    FCount  : integer;
    function getLocal(const index: integer): TLocal;
    procedure setLocal(const index: integer; const Value: TLocal);
    function getCount: integer;
    function getLast: TLocal;
    procedure IncreaseCapacity;
    procedure SetCount(const Value: integer);
  public
    //procedure Remove(const Index : integer);
    function Add(const name : String; const token : TToken) : TLocal;
    Constructor create;
    Destructor Destroy; override;
    property Item[const index : integer] : TLocal read getLocal write setLocal;default;
    property Last : TLocal read getLast;
    property Count : integer read getCount write SetCount;
  end;





implementation
uses
  sysutils;



{ TLocals }

function TLocals.Add(Const Name : String; const Token : TToken): TLocal;
begin
  if FCount = FCapacity then increaseCapacity;

  assert(assigned(token), 'token added to local is nil');

  FItems[FCount].Name := Name;

  FItems[FCount].Token := Token;

  FItems[FCount].Depth := -1;

  FItems[FCount].isCaptured := False;

  FCount := FCount + 1;
end;

constructor TLocals.create;
begin
  FCount := 0;
  FCapacity := Local_Capacity;
  SetLength(FItems,FCapacity);

end;

destructor TLocals.Destroy;
begin


  inherited;
end;

function TLocals.getCount: integer;
begin
  result := FCount;
end;

function TLocals.getLast: TLocal;
begin
  result := FItems[Count-1];
end;

function TLocals.getLocal(const index: integer): TLocal;
begin
  result := FItems[Index];
end;

procedure TLocals.IncreaseCapacity;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems,FCapacity);
end;

procedure TLocals.SetCount(const Value: integer);
begin
  assert(Value >=0, 'value is < 0');
  assert(Value < FCapacity, 'value is > Capacity');
  FCount := Value;
end;

(*procedure TLocals.Remove(const Index: integer);
var
  Local : TLocal;
begin
  assert(Index >=0, 'index is not > zero');
  assert(Index < FItems.count , 'index is not < count');

  Local := FItems[Index];
  FItems.Delete(index);
  DisposeLocal(Local);
end; *)

procedure TLocals.setLocal(const index: integer; const Value: TLocal);
begin
  FItems[Index] := Value;
end;

{ TLocal }

(*
constructor TLocal.Create(const name : string; const token: TToken);
begin
  inherited create;
  FName  := name;
  FToken := Token;
  FDepth := 0;
  FIsCaptured := False;
end;

destructor TLocal.destroy;
begin

  inherited;
end;

function TLocal.getToken: TToken;
begin
  result := FToken;
end;
           *)


end.
