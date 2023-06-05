unit charIterator;

interface
uses
  LoxTypes;

type
  TIterator = record
    text : string;
    TextLength : integer;
    index : integer;
    current : Char;

    function Move(const amount : integer) : boolean;
    function Seek(Const position : integer) : char;
    function SubStr(const start,finish : integer) : String;
    function PeekNext : char;
    function PeekPrior : char;
    function PeekTo(Const position : integer) : char;
    function HasNext : boolean;
    function HasPrev : boolean;
    function Next : char;
    function Prev : char;
    function First : char;
    function Last : char;
    procedure Init(const txt : String);
  end;



implementation


function TIterator.Seek(Const position : integer) : char;
begin
  result := cNull;
  if (TextLength = 0) or (position < 1) or (position > TextLength) then exit;
  Index := Position;
  result := text[index];
end;


function TIterator.PeekTo(Const position : integer) : char;
begin
  result := cNull;
  if (TextLength = 0) or (position < 1) or (position > TextLength) then exit;
  result := text[position];
end;

function Titerator.First : char;
begin
  result := cNull;
  if (TextLength = 0) then exit;
  index := 1;
  result := text[index];
  current := result;
end;

procedure Titerator.Init(const txt : String);
begin
  text := txt;
  index := 0;
  TextLength := Length(Text);
  current := cNull;
end;

function Titerator.HasNext : boolean;
begin
  result := index < TextLength;
end;

function Titerator.HasPrev : boolean;
begin
  result := index > 1;
end;

function Titerator.Prev : char;
begin
  result := cNull;
  if not hasPrev then exit;
  dec(index);
  result := text[index];
  current := result;
end;

function Titerator.PeekPrior : char;
begin
  result := cNull;
  if not hasPrev then exit;
  result := text[index-1];
end;

function Titerator.PeekNext : char;
begin
  result := cNull;
  if not hasNext then exit;
  result := text[index+1];
end;

function Titerator.Next : char;
begin
  result := cNull;
  if not hasNext then exit;
  inc(index);
  result := text[index];
  current := result;
end;

function Titerator.Last : char;
begin
  result := cNull;
  if TextLength = 0 then exit;
  index := TextLength;
  result := text[index];
  current := result;
end;

function TIterator.Move(const amount: integer): boolean;
begin
  result := false;
  if TextLength = 0 then exit;
  if index + amount > TextLength then exit;
  if index + amount < 1 then exit;
  index := Index + Amount;
  current := text[index];
  result := true;
end;

function TIterator.SubStr(const start,finish : integer) : String;
var
  i : integer;
begin
  result := '';
  if (Start+finish > TextLength) then exit;
  for i := start to finish do
  begin
    result := result + text[i];
  end;
end;

end.
