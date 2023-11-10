unit TokenArray;

interface

uses
  classes,sysutils,
  LOXTypes;

type

 TTokens = class
 private
   FItems : TList;
    function Count: integer;
    function getToken(const index: integer): TToken;
    procedure setToken(const index: integer; const Value: TToken);

 protected

 public

   procedure Add(const token : TToken);
   constructor create;
   destructor destroy;Override;
     
   property Item[const index : integer] : TToken read getToken write setToken; default;
 end;


 TTokenIterator = record
 private
   FIndex : integer;
   FTokens : TTokens;
   FCurrent : TToken;
   FPrevious : TToken;
 public
   function Index : integer;
   function Current : TToken;
   function Previous : TToken;

   function Count : integer;
   function MoveFirst : TToken;
   function MoveNext  : TToken;
   function peekNext : TToken;
   function MovePrev  : TToken;
   function PeekPrev : TToken;
   function MoveLast  : TToken;
   procedure init(const Tokens : TTokens);
 end;



implementation


procedure TTokens.setToken(const index: integer; const Value: TToken);
begin
  //FItems[index] := Value;
end;



procedure TTokens.Add(const token : TToken);
begin
  assert(token <> nil, 'token being inserted is nil');
  FItems.Add(token);
end;

function TTokens.Count: integer;
begin
  result := FItems.Count;
end;

constructor TTokens.create;
begin
  FItems := TList.Create;
end;

destructor TTokens.destroy;
begin
  FItems.Free;
  inherited;
end;



function TTokens.getToken(const index: integer): TToken;
begin
  result := FItems[index];
end;



function TTokenIterator.Count : integer;
  begin
    result := FTokens.Count;
  end;

  function TTokenIterator.MoveFirst : TToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := 0;
    FCurrent := FTokens[FIndex];
    FPrevious := nil;
    result := FCurrent;
  end;

  function TTokenIterator.MoveLast  : TToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    FIndex := FTokens.Count-1;
    FCurrent := FTokens[FIndex];
    FPrevious := PeekPrev;
    result := FCurrent;
  end;

  function TTokenIterator.MoveNext  : TToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;

    if FIndex = -1 then
    begin

      result := MoveFirst;
      //FPrevious := Current;
      exit;
    end;

    inc(FIndex);
    if FIndex < FTokens.Count then
    begin
      FPrevious := FCurrent;
      FCurrent := FTokens[FIndex];
      result := FCurrent;
    end;
  end;

  function TTokenIterator.MovePrev  : TToken;
  begin
    result := nil;
    if not FTokens.Count > 0 then exit;
    if FIndex > 0 then
    begin
      dec(FIndex);
      FCurrent := FTokens[FIndex];
      FPrevious := PeekPrev;
      result := FCurrent;
    end;
  end;


  function TTokenIterator.peekNext : TToken;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    inc(i);
    if i < FTokens.Count then
    begin
      result := FTokens[i];
    end;
  end;

  function TTokenIterator.PeekPrev : TToken;
  var
    i : integer;
  begin
    result := nil;
    i := FIndex;
    dec(i);
    if i >= 0 then
    begin
      result := FTokens[i];
    end;
  end;


    function TTokenIterator.Previous : TToken;
    begin
       result := FPrevious;
    end;

   function TTokenIterator.Current : TToken;
   begin
     result := FCurrent;
   end;

    function TTokenIterator.Index : integer;
    begin
      result := FIndex;
    end;

  procedure TTokenIterator.init(const Tokens : TTokens);
  begin
    FTokens := Tokens;
    FIndex := -1;
    FCurrent := nil;
    FPrevious := nil;
  end;

end.
