unit locals;

interface

uses
  classes,LOXTypes;

Const
  MAX_LOCALS = 256;


type


  TLocal = class
  private
    FName  : String;
    FIndex : integer;
    FToken : TToken;
    FDepth : integer;
    FIsCaptured : Boolean;
    function getToken: TToken;
  protected

  public
    Constructor Create(Const Name : String; const token : TToken);
    destructor destroy; override;
    procedure ToStrings(const strings : TStrings);
    property Token : TToken read getToken;// write setToken;
    property Index : Integer read FIndex;
    property Depth : integer read FDepth write FDepth;
    property IsCaptured : Boolean read FIsCaptured write FIsCaptured;
    property Name : string read fName;
  end;


  TLocals = class
  private
    FItems : TList;
    function getLocal(const index: integer): TLocal;
    procedure setLocal(const index: integer; const Value: TLocal);
    function getCount: integer;
    function getLast: TLocal;
  public
    procedure Remove(const Index : integer);
    function Add(const name : String; const token : TToken) : TLocal;
    Constructor create;
    Destructor Destroy; override;
    property Item[const index : integer] : TLocal read getLocal write setLocal;default;
    property Last : TLocal read getLast;
    property Count : integer read getCount;
  end;





implementation
uses
 sysutils;


////dumb, returns nothing...
function DisposeLocal(value : TLocal) : boolean;
begin
  assert(assigned(Value), ' value for disposal is nil');
  Value.Free;
end; 




{ TLocals }

function TLocals.Add(Const Name : String; const token : TToken): TLocal;
begin
  assert(assigned(token), 'token added to local is nil');
  result := TLocal.Create(Name,token);
  result.FName := Name;
  result.FIndex := Count;
  FItems.Add(result);
end;

constructor TLocals.create;
begin
  FItems := TList.create;
end;

destructor TLocals.Destroy;
var
  i : integer;
begin

  for i := FItems.Count-1 downto 0 do
  begin
    TLocal(FItems[i]).free;
  end;
  FItems.Free;
  inherited;
end;

function TLocals.getCount: integer;
begin
  result := FItems.count;
end;

function TLocals.getLast: TLocal;
begin
  result := FItems[Count-1];
end;

function TLocals.getLocal(const index: integer): TLocal;
begin
  result := FItems[Index];
end;

procedure TLocals.Remove(const Index: integer);
var
  Local : TLocal;
begin
  assert(Index >=0, 'index is not > zero');
  assert(Index < FItems.count , 'index is not < count');

  Local := FItems[Index];
  FItems.Delete(index);
  DisposeLocal(Local);
end;

procedure TLocals.setLocal(const index: integer; const Value: TLocal);
begin
  FItems[Index] := Value;
end;

{ TLocal }

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

procedure TLocal.ToStrings(const strings : TStrings);
const s = 'Index : %s, Depth = %s, IsCaptured : %s';
var
  txt : string;
begin
  (*
      Token : TToken;
    Depth : integer;
    IsCaptured : Boolean;
  *)
  txt := format(s,[inttostr(index),inttostr(depth),booltostr(IsCaptured)]);
  strings.add(txt);
  //Token.ToStrings(strings);

end;

end.
