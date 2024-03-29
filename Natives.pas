unit Natives;

interface

uses
  sysutils, Classes, Values;

  function DateTime(const ArgCount: Integer;const Values : TStack): TValueRecord;

  function FileExists(const ArgCount: Integer;const Values : TStack) : TValueRecord;

  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : TValueRecord;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : TValueRecord;


implementation
uses
  Dialogs, ValueManager;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TStack) : TValueRecord;
  var
    dlg : TOpenDialog;
  begin
   (* result := bc.newString('');
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


  function LoadStringFromFile(const ArgCount: Integer;const Values : TStack) : TValueRecord;
  var
    strings : TStrings;
    FileName : String;

  begin
    (*result := bc.newString(rVM,'');
    FileName := GetString(Values.Peek);
    if not SysUtils.FileExists(Filename) then exit;

    Strings := TStringList.create;
    try
      Strings.LoadFromFile(FileName);
      SetString(result,Strings.Text);
    finally
      Strings.Free;
    end; *)
  end;

  function FileExists(const ArgCount: Integer;const Values : TStack) : TValueRecord;
  var
    fileName : string;
    exists : Boolean;
  begin
    (*FileName := GetString(Values.Peek);
    result := bc.newBool(False);

    if SysUtils.FileExists(Filename) then SetBoolean(result,true);
    *)
    
  end;

  function DateTime(const ArgCount: Integer;const Values : TStack): TValueRecord;
  begin
     result := bc.newString(DateTimeToStr(Now));
  end;

end.
