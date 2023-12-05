unit Natives;

interface

uses
  sysutils, Classes, Values;

  function DateTime(const ArgCount: Integer;const Values : TStack): pValue;

  function FileExists(const ArgCount: Integer;const Values : TStack) : pValue;

  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : pValue;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : pValue;


implementation
uses
  Dialogs, ValueManager;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : pValue;
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


  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : pValue;
  var
    strings : TStrings;
    FileName : String;

  begin
    result := BorrowChecker.newString('');
    FileName := Values.Peek(0).toString;
    if not SysUtils.FileExists(Filename) then exit;

    Strings := TStringList.create;
    try
      Strings.LoadFromFile(FileName);
      result.Str := Strings.Text;
    finally
      Strings.Free;
    end;
  end;

  function FileExists(const ArgCount: Integer;const Values : TStack) : pValue;
  var
    fileName : string;
    exists : Boolean;
  begin
    FileName := Values.Peek(0).toString;
    result := BorrowChecker.newBool(False);

    if SysUtils.FileExists(Filename) then result.Boolean := true;

  end;

  function DateTime(const ArgCount: Integer;const Values : TStack): pValue;
  begin
     result := BorrowChecker.newString(DateTimeToStr(Now));
  end;

end.
