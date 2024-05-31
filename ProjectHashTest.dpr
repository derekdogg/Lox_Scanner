program ProjectHashTest;

{$APPTYPE CONSOLE}

uses
  dialogs,
  SysUtils,
  Dictionary in 'Dictionary.pas';

var
  Dictionary: TDictionary;

procedure Test;
var
   a,b,c,d,e : TKeyValue;
   f,g,h,i,j : TKeyValue;
   idx : integer;
begin

   a.Name := 'a';
   b.Name := 'b';
   c.Name := 'c';
   d.Name := 'd';
   e.Name := 'e';

   Assert(Dictionary.Add(a) <> -1, 'not added');
   Assert(Dictionary.Add(b) <> -1, 'not added');
   Assert(Dictionary.Add(c) <> -1, 'not added');
   Assert(Dictionary.Add(d) <> -1, 'not added');
   Assert(Dictionary.Add(e) <> -1, 'not added');

   idx := Dictionary.IndexOf(a);
   Assert(idx  <> -1, 'Could not retrieve A');
   Assert(Dictionary[idx].name = 'a', 'expected a');

   idx := Dictionary.IndexOf(b);
   Assert(idx  <> -1, 'Could not retrieve b');
   Assert(Dictionary[idx].name = 'b', 'expected b');

   idx := Dictionary.IndexOf(c);
   Assert(idx  <> -1, 'Could not retrieve c');
   Assert(Dictionary[idx].name = 'c', 'expected c');

   idx := Dictionary.IndexOf(d);
   Assert(idx  <> -1, 'Could not retrieve d');
   Assert(Dictionary[idx].name = 'd', 'expected d');

   idx := Dictionary.IndexOf(e);
   Assert(idx  <> -1, 'Could not retrieve e');
   Assert(Dictionary[idx].name = 'e', 'expected e');


   Assert(Dictionary.Remove(a) = true,'not true deleting a');
   Assert(Dictionary.Remove(b) = true,'not true deleting b');
   Assert(Dictionary.Remove(c) = true,'not true deleting c');
   Assert(Dictionary.Remove(d) = true,'not true deleting d');
   Assert(Dictionary.Remove(e) = true,'not true deleting e');

   Assert(Dictionary.Add(a) <> -1, 'not added');
   Assert(Dictionary.Add(b) <> -1, 'not added');
   Assert(Dictionary.Add(c) <> -1, 'not added');
   Assert(Dictionary.Add(d) <> -1, 'not added');
   Assert(Dictionary.Add(e) <> -1, 'not added');

   idx := Dictionary.IndexOf(a);
   Assert(idx  <> -1, 'Could not retrieve A');
   Assert(Dictionary[idx].name = 'a', 'expected a');

   idx := Dictionary.IndexOf(b);
   Assert(idx  <> -1, 'Could not retrieve b');
   Assert(Dictionary[idx].name = 'b', 'expected b');

   idx := Dictionary.IndexOf(c);
   Assert(idx  <> -1, 'Could not retrieve c');
   Assert(Dictionary[idx].name = 'c', 'expected c');

   idx := Dictionary.IndexOf(d);
   Assert(idx  <> -1, 'Could not retrieve d');
   Assert(Dictionary[idx].name = 'd', 'expected d');

   idx := Dictionary.IndexOf(e);
   Assert(idx  <> -1, 'Could not retrieve e');
   Assert(Dictionary[idx].name = 'e', 'expected e');

  // Assert(Dictionary.Remove(e) = false,'not true deleting a - already removed');
 
end;




procedure RunTests;
begin
  try

     Test ;

     writeln('Test Success: press key to close');
  except
    on E: Exception do
      Writeln('Test failed: ', E.Message);
  end;
end;

begin
  Dictionary := TDictionary.Create;
  try
    RunTests;
  finally
    Dictionary.Free;
    Readln;
  end;

end.

