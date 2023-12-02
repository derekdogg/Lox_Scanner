unit Natives;

interface

uses
  sysutils, Classes, Values;

  function DateTime(const ArgCount: Integer;const Values : TValueStack): pValue;

  function FileExists(const ArgCount: Integer;const Values : TValueStack) : pValue;

  function LoadStringFromFile(const ArgCount: Integer;const Values : TValueStack) : pValue;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TValueStack) : pValue;


implementation
uses
  Dialogs;

  function OpenFileAndLoad(const ArgCount: Integer;const Values : TValueStack) : pValue;
  var
    dlg : TOpenDialog;
  begin
    result := newString('');
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
    end;
  end;


  function LoadStringFromFile(const ArgCount: Integer;const Values : TValueStack) : pValue;
  var
    strings : TStrings;
    FileName : String;

  begin
    result := newString('');
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

  function FileExists(const ArgCount: Integer;const Values : TValueStack) : pValue;
  var
    fileName : string;
    exists : Boolean;
  begin
    FileName := Values.Peek(0).toString;
    result := newBool(False);

    if SysUtils.FileExists(Filename) then result.Boolean := true;

  end;

  function DateTime(const ArgCount: Integer;const Values : TValueStack): pValue;
  begin
     result := newString(DateTimeToStr(Now));
  end;

end.
