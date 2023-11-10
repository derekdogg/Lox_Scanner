unit Natives;

interface

uses
  sysutils, Values;


  function DateTime(const ArgCount: Integer;const Values : TValueStack): pValue;

implementation

  function DateTime(const ArgCount: Integer;const Values : TValueStack): pValue;
  begin
     result := newString(DateTimeToStr(Now));
  end;

end.
