unit locals;

interface

uses
  classes,LOXTypes;

Const
  MAX_LOCALS = 256;
  MAX_CAPACITY = 1000; //lets keep it reasonable - note this is not the size of the allocated array pNameValueItems.
  INCREMENT_CAPACITY_BY = 2;
  NUM_SLOTS = 10;
  GROWTH_FACTOR  = 2;

type


  TLocal = class
  private
    FName  : String;
    FIndex : integer;
    FToken : TToken;
    FDepth : integer;
    FIsCaptured : Boolean;
    function getToken: TToken;
    procedure setToken(const Value: TToken);
  protected

  public
    Constructor Create(Const Name : String; const token : TToken);
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

  protected


  public
    procedure ToString(const Strings : TStrings);
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
begin
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

procedure TLocals.ToString(const Strings: TStrings);
var
  i : integer;
  Local : TLocal;
begin
  assert(Assigned(Strings),'no strings assigned for print to');
  for i := 0 to FItems.Count-1 do
  begin
    Local := FItems[i];

    Local.ToStrings(Strings);
  end;
end;

procedure TLocals.Remove(const Index: integer);
var
  Local : TLocal;
begin
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

function TLocal.getToken: TToken;
begin
  result := FToken;
end;

procedure TLocal.setToken(const Value: TToken);
begin
  assert(assigned(value), 'token is nil for setting a local');
  FToken := Value;
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
