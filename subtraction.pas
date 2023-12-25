 unit Subtraction;


interface
uses
  dialogs,values, sysutils;

  function Subtract(const L,R : TValueRecord) : TValueRecord;



implementation
uses
  ValueManager;





function  LeftNullSubtraction(const L,R : TValueRecord) : TValueRecord;
begin
  (*result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;
  if not GetIsNull(L) then exit;
  if GetIsNull(R) then exit;
  result := R; *)
end;

  function RightNullSubtraction(const L,R : TValueRecord) : TValueRecord;
begin
  (*result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsNull(R) then exit;
  if GetIsNull(L) then exit;
  result := L; *)
end;

 function  BothNullSubtraction(const L,R : TValueRecord) : TValueRecord;
begin
 (* result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsNull(L) then exit;
  if not GetIsNull(R) then exit;

  result := R;   *)
end;


  function  SubtractNulls(const L,R : TValueRecord) : TValueRecord;
begin
  (*result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;


  result := TNullSubtraction.LeftNullSubtraction(L,R);
  if assigned(result) then exit;

  result := TNullSubtraction.RightNullSubtraction(L,R);
  if assigned(result) then exit;

  result := TNullSubtraction.BothNullSubtraction(L,R);
  if assigned(result) then exit;  *)
end;

function SubtractNumbers(const L, R: TValueRecord): TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  if not GetIsNumber(L) then exit;
  if not GetIsNumber(R) then exit;

  result := BorrowChecker.NewNumber(GetNumber(L) - GetNumber(R));
end;




function SubtractStrings(const L, R: TValueRecord): TValueRecord;
begin
  (*result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := TStringSubtraction.SubtractStrings(L,R);
  if assigned(result) then exit;

  result := TStringSubtraction.SubtractNumber(L,R);
  if assigned(result) then exit;

  raise exception.create('cant Subtract - TSubtraction');
  *)
end;

function Subtract(const L,R : TValueRecord) : TValueRecord; overload;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  //result := SubtractNulls(L,R);
  //if assigned(result) then exit;

  result := SubtractNumbers(L,R);
  //if assigned(result) then exit;

  //result := SubtractStrings(L,R);
  //if assigned(result) then exit;

  //raise exception.create('cant Subtract - TSubtraction');
end;


{ TStringSubtraction }

function Subtract(original, toSubtract: string): string; overload;
var
  i, j: Integer;
  resultString: string;
begin
  resultString := original;

  for i := 1 to Length(toSubtract) do
  begin
    j := Pos(toSubtract[i], resultString);
    if j > 0 then
      System.Delete(resultString, j, 1);
  end;

  Result := resultString;
end;


function SubtractNumber(const L,R: TValueRecord): TValueRecord;
var
  s : string;

begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  if GetIsString(L) then
  begin
    s := copy(GetString(L),0, Length(GetString(L)) -1 - round(GetNumber(R)));
    result := BorrowChecker.NewString(rVM,s);
    exit;
  end;


end;


end.
