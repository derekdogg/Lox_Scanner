 unit Subtraction;


interface
uses
  dialogs,values, sysutils;

type

  TNullSubtraction = class
    class function LeftNullSubtraction(const L,R : pValueRecord) : pValueRecord;
    class function RightNullSubtraction(const L,R : pValueRecord) : pValueRecord;
    class function BothNullSubtraction(const L,R : pValueRecord) : pValueRecord;
  end;

 TStringSubtraction = class
    class function SubtractStrings(const L,R : pValueRecord) : pValueRecord;
    class function SubtractNumber(const L,R : pValueRecord) : pValueRecord;
 //   class function SubtractNull(const L,R : pValueRecord) : pValueRecord;
  end;


  TSubtraction = class
     class function SubtractNulls(const L,R : pValueRecord) : pValueRecord;
     class function SubtractNumbers(const L,R : pValueRecord) : pValueRecord;
     class function SubtractStrings(const L,R : pValueRecord) : pValueRecord;
     class function Subtract(const L,R : pValueRecord) : pValueRecord;
  end;


implementation
uses
  ValueManager;


class function TNullSubtraction.LeftNullSubtraction(const L,R : pValueRecord) : pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;
  if not GetIsNull(L) then exit;
  if GetIsNull(R) then exit;
  result := R;
end;

class function TNullSubtraction.RightNullSubtraction(const L,R : pValueRecord) : pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsNull(R) then exit;
  if GetIsNull(L) then exit;
  result := L;
end;

class function TNullSubtraction.BothNullSubtraction(const L,R : pValueRecord) : pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsNull(L) then exit;
  if not GetIsNull(R) then exit;

  result := R;
end;


class function TSubtraction.SubtractNulls(const L,R : pValueRecord) : pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;


  result := TNullSubtraction.LeftNullSubtraction(L,R);
  if assigned(result) then exit;

  result := TNullSubtraction.RightNullSubtraction(L,R);
  if assigned(result) then exit;

  result := TNullSubtraction.BothNullSubtraction(L,R);
  if assigned(result) then exit;
end;

class function TSubtraction.SubtractNumbers(const L, R: pValueRecord): pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsNumber(L) then exit;
  if not GetIsNumber(R) then exit;

  result := BorrowChecker.NewNumber(GetNumber(L) - GetNumber(R));
end;




class function TSubtraction.SubtractStrings(const L, R: pValueRecord): pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := TStringSubtraction.SubtractStrings(L,R);
  if assigned(result) then exit;

  result := TStringSubtraction.SubtractNumber(L,R);
  if assigned(result) then exit;

  raise exception.create('cant Subtract - TSubtraction');

end;

class function TSubtraction.Subtract(const L,R : pValueRecord) : pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := SubtractNulls(L,R);
  if assigned(result) then exit;

  result := SubtractNumbers(L,R);
  if assigned(result) then exit;

  result := SubtractStrings(L,R);
  if assigned(result) then exit;

  raise exception.create('cant Subtract - TSubtraction');
end;


{ TStringSubtraction }

function Subtract(original, toSubtract: string): string;
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


class function TStringSubtraction.SubtractNumber(const L,R: pValueRecord): pValueRecord;
var
  s : string;

begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if GetIsString(L) then
  begin
    s := copy(GetString(L),0, Length(GetString(L)) -1 - round(GetNumber(R)));
    result := BorrowChecker.NewString(rVM,s);
    exit;
  end;


end;

class function TStringSubtraction.SubtractStrings(const L, R: pValueRecord): pValueRecord;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsString(L) then exit;
  if not GetIsString(R) then exit;

  result := BorrowChecker.NewString(rVM,Subtract(GetString(L),GetString(R)));

end;

end.
