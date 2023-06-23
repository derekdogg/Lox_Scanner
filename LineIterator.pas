unit LineIterator;

interface

uses
  loxtypes,charIterator;

type

  TLine = record
    start : integer;
    stop  : integer;
    text  : string;
    count : integer;
  end;

  TLines = array[0..cMaxLines] of TLine;

  TLineProcedure = procedure(Const Line : TLine);


  TLineIterator = record
    Text : String;
    chars : TIterator;
    Items : TLines;
    LineCount : integer;
    LineIndex : integer;
    function EOL : boolean;
    Function LineRecordIsNull(Const Rec : TLine) : Boolean;
    procedure Init(const txt : string);
    procedure BuildIndex(const text : string);
    function  HasNext : boolean;
    Function  First :  TLine;
    function  Next : TLine;
  end;

implementation

uses sysutils;

function TLineIterator.EOL: boolean;
begin
  result := LineIndex >= chars.TextLength;
end;

function TLineIterator.First : TLine;
  var
    Line : TLine;
  begin
    fillchar(Line,sizeof(Line),cNull);
    if not LineCount > 0 then
    begin
      result := Line;
      exit;
    end;
    LineIndex := 0;
    if LineCount > 0 then
    begin
      result := items[LineIndex];
    end;
  end;

  function TLineIterator.HasNext : boolean;
  begin
    result := LineIndex < LineCount;
  end;

  function TLineIterator.Next : TLine;
  var
    Line : TLine;
  begin
    fillchar(Line,sizeof(Line),cNull);
    if not LineCount > 0 then
    begin
      result := Line;
      exit;
    end;
    inc(LineIndex);
    result := items[LineIndex];
  end;




  //this is probably super inneficient, but I didn't want to use delphi baked in code.
  //because at a later stage I have plans to move it to C
  procedure TLineIterator.BuildIndex(const text : string);
  var
    CurrentChar : string;
    NextChar : string;
    prevIdx, StopIdx : integer;
  begin
     init(text);
     LineCount := 0;
     PrevIdx := 1;
       While (chars.HasNext) do
       begin
         CurrentChar := chars.Next;
         NextChar := chars.PeekNext;
         if (CurrentChar = cCR) and (NextChar = cLF) then
         begin
           StopIdx  := chars.index-1; //the char before the test of cCR

           items[LineCount].start := PrevIdx; //currently this is an index into the text, not per line
           items[LineCount].stop := StopIdx;
           items[LineCount].count := StopIdx-PrevIdx+1;
           items[LineCount].text := Copy(Text,PrevIdx,StopIdx-PrevIdx+1);
           prevIdx := StopIdx+3;  //char before cCR + 2 for the CR and LF
           inc(LineCount);
         end;
       end;
       //string with no return and line feed is still a line
       if (chars.Last <> cLF) and (chars.PeekPrior <> cCR) then
       begin
         items[LineCount].start := PrevIdx;
         items[LineCount].stop := chars.TextLength;
         items[LineCount].count := chars.TextLength-PrevIdx+1;
         items[LineCount].text := Copy(Text,PrevIdx,chars.TextLength-PrevIdx+1);
         inc(LineCount);
       end;
  end;

  procedure TLineIterator.Init(const txt : string);
  begin
    Text := Txt;
    chars.Init(txt);
    LineIndex := -1;
    fillchar(items, Sizeof(items),cNull);
  end;

  Function TLineIterator.LineRecordIsNull(Const Rec : TLine) : Boolean;
  begin
    result := (rec.count = 0) or (length(trim(rec.text)) = 0);
  end;


end.
