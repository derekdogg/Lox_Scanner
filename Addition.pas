unit Addition;


interface
uses
  dialogs,values, sysutils;

type

  TNullAddition = class
    class function LeftNullAddition(const L,R : TValueRecord) : TValueRecord;
    class function RightNullAddition(const L,R : TValueRecord) : TValueRecord;
    class function BothNullAddition(const L,R : TValueRecord) : TValueRecord;
  end;

  TStringAddition = class
    class function AddTwoStrings(const L,R : TValueRecord) : TValueRecord;
    class function AddNumberToString(const L,R : TValueRecord) : TValueRecord;
    class function AddNullToString(const L,R : TValueRecord) : TValueRecord;

  end;


  TAddition = class
     class function AddNulls(const L,R : TValueRecord) : TValueRecord;
     class function AddNumbers(const L,R : TValueRecord) : TValueRecord;
     class function AddStrings(const L,R : TValueRecord) : TValueRecord;
     class function add(const L,R : TValueRecord) : TValueRecord;
  end;


implementation
uses
  ValueManager;


class function TNullAddition.LeftNullAddition(const L,R : TValueRecord) : TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;
  if not GetIsNull(L) then exit;
  if GetIsNull(r) then exit;
  result := R;
end;

class function TNullAddition.RightNullAddition(const L,R : TValueRecord) : TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  if not getIsNull(R) then exit;
  if getIsNull(L) then exit;
  result := L;
end;

class function TNullAddition.BothNullAddition(const L,R : TValueRecord) : TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  if not GetIsNull(L) then exit;
  if not GetIsNull(R) then exit;

  result := R;
end;


class function TAddition.AddNulls(const L,R : TValueRecord) : TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;
  exit;

  (*
  result := TNullAddition.LeftNullAddition(L,R);
  if assigned(result) then exit;

  result := TNullAddition.RightNullAddition(L,R);
  if assigned(result) then exit;

  result := TNullAddition.BothNullAddition(L,R);
  if assigned(result) then exit; *)
end;

class function TAddition.AddNumbers(const L, R: TValueRecord): TValueRecord;
begin
 // result := nil;
 // if not assigned(L) then exit;
 // if not assigned(R) then exit;

  if not GetIsNumber(L) then exit;
  if not GetIsNumber(R) then exit;

  result := BorrowChecker.NewNumber(L.Number + R.Number);
end;




class function TAddition.AddStrings(const L, R: TValueRecord): TValueRecord;
begin
//  result := nil;
//  if not assigned(L) then exit;
//  if not assigned(R) then exit;
  exit;

  (*

  result := TStringAddition.AddTwoStrings(L,R);
  if assigned(result) then exit;

  result :=TStringAddition.AddNumberToString(L,R);
  if assigned(result) then exit;

  result :=TStringAddition.AddNullToString(L,R);
  if assigned(result) then exit;
  *)
end;

class function TAddition.Add(const L,R : TValueRecord) : TValueRecord;
begin
  //result := nil;
  //if not assigned(L) then exit;
  //if not assigned(R) then exit;

  //result := AddNulls(L,R);
  //if assigned(result) then exit;

  result := AddNumbers(L,R);
//  if assigned(result) then exit;

  //result := AddStrings(L,R);
  //if assigned(result) then exit;
  //showmessage('er');
  //raise exception.create('cant add');
end;


{ TStringAddition }
class function TStringAddition.AddNullToString(const L, R: TValueRecord): TValueRecord;
begin
//  result := nil;
//  if not assigned(L) then exit;
//  if not assigned(R) then exit;

  if GetIsNull(L) or GetIsNull(R) then
  begin
    result := BorrowChecker.newString(rVm,GetString(L) + GetString(R));
  end;


end;

class function TStringAddition.AddNumberToString(const L, R: TValueRecord): TValueRecord;
begin
(*  result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if GetIsString(L) then
  begin
    result := BorrowChecker.newString(rVm,GetString(L) + GetString(R));
    exit;
  end;

  if GetIsString(R) then
  begin
    result := BorrowChecker.newString(rVM,GetString(L) + GetString(R));
    exit;
  end; *)
end;

class function TStringAddition.AddTwoStrings(const L, R: TValueRecord): TValueRecord;
begin
  (*result := nil;
  if not assigned(L) then exit;
  if not assigned(R) then exit;

  if not GetIsString(L)  then exit;
  if not GetIsString(R) then exit;

  result := BorrowChecker.NewString(rVM,GetString(L) + GetString(R));
  *)
end;

end.
