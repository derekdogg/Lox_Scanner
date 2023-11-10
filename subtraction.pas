 unit Subtraction;


interface
uses
  dialogs,values, sysutils;

type

  TNullSubtraction = class
    class function LeftNullSubtraction(const L,R : pValue) : pValue;
    class function RightNullSubtraction(const L,R : pValue) : pValue;
    class function BothNullSubtraction(const L,R : pValue) : pValue;
  end;

 TStringSubtraction = class
    class function SubtractStrings(const L,R : pValue) : pValue;
    class function SubtractNumber(const L,R : pValue) : pValue;
 //   class function SubtractNull(const L,R : pValue) : pValue;
  end;


  TSubtraction = class
     class function SubtractNulls(const L,R : pValue) : pValue;
     class function SubtractNumbers(const L,R : pValue) : pValue;
     class function SubtractStrings(const L,R : pValue) : pValue;
     class function Subtract(const L,R : pValue) : pValue;
  end;


implementation


class function TNullSubtraction.LeftNullSubtraction(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;
  if not L.IsNull then exit;
  if r.IsNull then exit;
  result := R;
end;

class function TNullSubtraction.RightNullSubtraction(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not R.IsNull then exit;
  if L.IsNull then exit;
  result := L;
end;

class function TNullSubtraction.BothNullSubtraction(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsNull then exit;
  if not R.IsNull then exit;

  result := R;
end;


class function TSubtraction.SubtractNulls(const L,R : pValue) : pValue;
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

class function TSubtraction.SubtractNumbers(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsNumber then exit;
  if not R.IsNumber then exit;

  result := NewNumber(L.Number - R.Number);
end;




class function TSubtraction.SubtractStrings(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := TStringSubtraction.SubtractStrings(L,R);
  if assigned(result) then exit;

  result := TStringSubtraction.SubtractNumber(L,R);
  if assigned(result) then exit;

//  result :=TStringSubtraction.SubtractNull(L,R);
//  if assigned(result) then exit;

end;

class function TSubtraction.Subtract(const L,R : pValue) : pValue;
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
  showmessage('er');
  //raise exception.create('cant Subtract');
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


class function TStringSubtraction.SubtractNumber(const L,R: pValue): pValue;
var
  s : string;

begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if L.IsString then
  begin
    s := copy(L.ToString,0, Length(L.toString) -1 - round(R.Number));
    result := NewString(s);
    exit;
  end;


end;

class function TStringSubtraction.SubtractStrings(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsString then exit;
  if not R.IsString then exit;

  result := NewString(Subtract(L.ToString,R.ToString));

end;

end.
