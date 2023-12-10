unit Natives;

interface

uses
  sysutils, Classes, Values;

  function DateTime(const ArgCount: Integer;const Values : TStack): pValueRecord;

  function FileExists(const ArgCount: Integer;const Values : TStack) : pValueRecord;

  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : pValueRecord;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : pValueRecord;


implementation
uses
  Dialogs, ValueManager;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : pValueRecord;
  var
    dlg : TOpenDialog;
  begin
   (* result := BorrowChecker.newString('');
    dlg := TOpenDialog.Create(nil);
    try
      if dlg.execute then
      begin
         Values.Push(NewString(Dlg.Filename));
         result := LoadStringFromFile(0,Values);
        Values.Pop;
      end;
    finally
      dlg.free;
    end; *)
  end;


  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : pValueRecord;
  var
    strings : TStrings;
    FileName : String;

  begin
    result := BorrowChecker.newString(rVM,'');
    FileName := GetString(Values.Peek(0));
    if not SysUtils.FileExists(Filename) then exit;

    Strings := TStringList.create;
    try
      Strings.LoadFromFile(FileName);
      SetString(result,Strings.Text);
    finally
      Strings.Free;
    end;
  end;

  function FileExists(const ArgCount: Integer;const Values : TStack) : pValueRecord;
  var
    fileName : string;
    exists : Boolean;
  begin
    FileName := GetString(Values.Peek(0));
    result := BorrowChecker.newBool(False);

    if SysUtils.FileExists(Filename) then SetBoolean(result,true);

  end;

  function DateTime(const ArgCount: Integer;const Values : TStack): pValueRecord;
  begin
     result := BorrowChecker.newString(rVM,DateTimeToStr(Now));
  end;

end.
