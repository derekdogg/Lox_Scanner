unit Addition;


interface
uses
  dialogs,values, sysutils;

type

  TNullAddition = class
    class function LeftNullAddition(const L,R : pValue) : pValue;
    class function RightNullAddition(const L,R : pValue) : pValue;
    class function BothNullAddition(const L,R : pValue) : pValue;
  end;

  TStringAddition = class
    class function AddTwoStrings(const L,R : pValue) : pValue;
    class function AddNumberToString(const L,R : pValue) : pValue;
    class function AddNullToString(const L,R : pValue) : pValue;

  end;


  TAddition = class
     class function AddNulls(const L,R : pValue) : pValue;
     class function AddNumbers(const L,R : pValue) : pValue;
     class function AddStrings(const L,R : pValue) : pValue;
     class function add(const L,R : pValue) : pValue;
  end;


implementation


class function TNullAddition.LeftNullAddition(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;
  if not L.IsNull then exit;
  if r.IsNull then exit;
  result := R;
end;

class function TNullAddition.RightNullAddition(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not R.IsNull then exit;
  if L.IsNull then exit;
  result := L;
end;

class function TNullAddition.BothNullAddition(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsNull then exit;
  if not R.IsNull then exit;

  result := R;
end;


class function TAddition.AddNulls(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;


  result := TNullAddition.LeftNullAddition(L,R);
  if assigned(result) then exit;

  result := TNullAddition.RightNullAddition(L,R);
  if assigned(result) then exit;

  result := TNullAddition.BothNullAddition(L,R);
  if assigned(result) then exit;
end;

class function TAddition.AddNumbers(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsNumber then exit;
  if not R.IsNumber then exit;

  result := NewNumber(L.Number + R.Number);
end;




class function TAddition.AddStrings(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := TStringAddition.AddTwoStrings(L,R);
  if assigned(result) then exit;

  result :=TStringAddition.AddNumberToString(L,R);
  if assigned(result) then exit;

  result :=TStringAddition.AddNullToString(L,R);
  if assigned(result) then exit;

end;

class function TAddition.Add(const L,R : pValue) : pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  result := AddNulls(L,R);
  if assigned(result) then exit;

  result := AddNumbers(L,R);
  if assigned(result) then exit;

  result := AddStrings(L,R);
  if assigned(result) then exit;
  showmessage('er');
  //raise exception.create('cant add');
end;


{ TStringAddition }
class function TStringAddition.AddNullToString(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if (L.IsNull) or (r.IsNull) then
  begin
    result := newString(L.ToString + R.ToString);
  end;


end;

class function TStringAddition.AddNumberToString(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if L.IsString then
  begin
    result := newString(L.ToString + R.ToString);
    exit;
  end;

  if R.IsString then
  begin
    result := newString(L.ToString + R.ToString);
    exit;
  end;
end;

class function TStringAddition.AddTwoStrings(const L, R: pValue): pValue;
begin
  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not L.IsString then exit;
  if not R.IsString then exit;

  result := NewString(L.ToString + R.ToString);

end;

end.
