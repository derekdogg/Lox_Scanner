unit TokenArray;

interface

uses
  LoxTypes;

type

  TToken = record
    Text        : string;
    Line        : Longint;
    pos         : longint;
    Kind        : TTokenKind;
    function isReserved : boolean;
    function KindToStr  : string;
  end;

  TTokens = array of TToken;

  TTokenIterator = record
  private
    FIndex    : integer;
    FTokens   : TTokens;
    FCount    : integer;
    procedure setIndex(const Value: integer);
    function GetIndex : integer;
  public
     procedure init(const Tokens : TTokens; const count : integer);

    function MoveNext  : integer;
    function Current : TToken;
    function Previous    : TToken;
    function count : integer;
    property Index  : integer read getIndex write setIndex;
    property Tokens : TTokens read FTokens;
  end;



implementation


function TTokenIterator.MoveNext  : integer;
begin
  result := -1;
  if FIndex < FCount-1 then
  begin
    inc(FIndex);
    result := FIndex;
  end;
end;


function TTokenIterator.count: integer;
begin
  result := fCount;
end;

function TTokenIterator.Current: TToken;
begin
  assert(FIndex >= 0);
  assert(FIndex < FCount);
  result := FTokens[FIndex];
end;

function TTokenIterator.GetIndex: integer;
begin
  result := FIndex;
end;

function TTokenIterator.Previous: TToken;
begin
  assert(FIndex > 0);
  assert(FIndex < FCount);
  result := FTokens[FIndex-1];
end;

procedure TTokenIterator.setIndex(const Value: integer);
begin
  assert(Value < FCount-1);
  assert(Value >= 0);
  FIndex := Value;
end;


procedure TTokenIterator.init(const Tokens: TTokens;const count : integer);
begin
  assert(Assigned(Tokens));
  FCount := Count;
  FIndex := -1;
  FTokens := Tokens;
end;

{ TToken }

function TToken.isReserved: boolean;
begin
  result := Kind in TReserved;
end;

function TToken.KindToStr: string;
begin
  result := TTokenName[Kind];
end;

 

end.
