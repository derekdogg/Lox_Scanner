unit scanner;

interface
uses
  sysutils,LoxTypes, charIterator,LineIterator;

type

  TScanner = record
    TokenIndex : integer;
    Tokens : TTokens;
    TokenCount : integer;
    ln : TLineIterator;

    function CurrentCharIsNumber : boolean;
    function charIsNumber(const c : char) : boolean;
    function MatchAny(const c : char; const Any : Array of TAscii) : boolean;
    function MatchChar(const c : char; const ascii : TAscii) : boolean;
    function Match(const Ascii : TAscii) : boolean;
    function MakeNumberToken : TToken;
    function NullToken : TToken;
    function GetReserverdWordToken(const TokenKind : TTokenKind) : TToken;
    function ReservedOrWord(tokenKind : TTokenKind) : TToken;
    function AllowableWordChar(Const c : char) : boolean;
    function MatchSpecialChar(const c : char) : boolean;

    function SkipWhiteSpace : boolean;
    function MakeWordToken : TToken;
    function MakeStringToken : TToken;
    function LineLength : Integer;
    function SpaceBeforeWordStart(const WordStart : integer) : boolean;
    function SpaceAfterWordEnd(const WordEnd : integer) : boolean;
    function OpenBracketAfterWordEnd(const WordEnd : integer) : boolean;
    function MatchKeyWord(const word : String) : boolean;
    function CheckKeyWord(const word : String) : boolean;
    function MakeToken : TToken;
    function CharInAsiiSubset : Boolean;
    function ScanLine(
      const LineRecord : TLine) : TTokens;
    procedure Init(const text : string);
    procedure Scan;
  end;


implementation
//uses
  //sysutils;



  procedure TScanner.Init(const text : string);
  begin
    ln.BuildIndex(Text);
    TokenIndex := -1;
    TokenCount := 0;
  end;



  function TScanner.LineLength : Integer;
  begin
    result := ln.chars.TextLength;
  end;

  function TScanner.SpaceBeforeWordStart(const WordStart : integer) : boolean;
  begin
    result := false;
    if (wordStart = 1) or (WordStart > LineLength) then
    begin
      exit;
    end;
    result := MatchChar(ln.chars.PeekTo(WordStart-1),space);
  end;

  function TScanner.SpaceAfterWordEnd(const WordEnd : integer) : boolean;
  begin
    result := false;
    if (wordEnd = LineLength) or (wordEnd<1) then
    begin
      exit;
    end;
    result := MatchChar(ln.chars.PeekTo(WordEnd+1),space);

  end;

  //this is fucking horible and needs revising.
  function TScanner.CheckKeyWord(Const word : String) : boolean;
  var wordStart, WordEnd : integer;
  begin
    wordStart := ln.chars.Index;
    WordEnd   := ln.chars.Index + Length(word)-1;

    result := false;

    if (wordStart < 1) or (wordStart > LineLength) or
       (WordEnd < 1) or (WordEnd > LineLength) or
       (WordEnd <= WordStart) then
    begin
      exit;
    end;

    if (wordStart = 1) and (WordEnd = LineLength) then //start of line       AND
    begin
       result := true;
       exit;
    end;

    if (wordStart = 1) and (WordEnd < LineLength) then //start of line       AND    12345678
    begin
       result := SpaceAfterWordEnd(WordEnd) or OpenBracketAfterWordEnd(WordEnd);
       exit;
    end;

    if (wordStart > 1) and(WordEnd < LineLength)  then //middle of line  12345AND12345678
    begin
      result := (SpaceAfterWordEnd(WordEnd) or OpenBracketAfterWordEnd(WordEnd)) and SpaceBeforeWordStart(wordStart);
      exit;
    end;

    if (wordStart > 1) and (WordEnd = LineLength) then
    begin
      result := SpaceBeforeWordStart(WordStart);
      exit;
    end;
  end;

function TScanner.MatchAny(const c: char; const Any: array of TAscii): boolean;
var
  i : integer;
begin
  result := false;
  for I := low(Any) to high(any) do
  begin
    if MatchChar(c,Any[i]) then
    begin
      result := true;
      break;
    end;
  end;
end;

function TScanner.MatchKeyword(const word : String) : boolean;
  var
    str : string;
  begin
    str := lowercase(ln.chars.Substr(ln.chars.Index, Length(word)));
    result := Str = lowercase(word);
  end;

function TScanner.OpenBracketAfterWordEnd(const WordEnd: integer): boolean;
begin
    if (wordEnd = LineLength) or (wordEnd<1) then
    begin
      result := false;
      exit;
    end;

    result := MatchChar(ln.chars.PeekTo(WordEnd+1),open_bracket);
end;


function TScanner.MatchChar(const c : char; const ascii : TAscii) : boolean;
begin
  result := ord(c) = Ord(ascii);
end;



function TScanner.MakeStringToken : TToken;
var
  start,
  idx : integer;
begin
  result := nullToken;

  if not Match(Quotes) then exit;

  start := ln.chars.index; idx := start;

  result.text := ln.chars.current;
  while (ln.chars.next <> cNull) and (Match(quotes) = false) do
  begin
    result.text := result.text + ln.chars.current;
    inc(idx);
  end;

  if Match(Quotes) then
  begin
     result.text := result.text + ln.chars.current;
     result.kind := tkQuotes;
     result.start := start;
     result.length := idx+1;

     exit;  //<=== exit here
  end;

  //unterminated string, i.e. the current char we're on is not a "
  result.text := result.text + ln.chars.current;
  result.kind := tkUnterminatedQuotes;
  result.start := start;
  result.length := ln.chars.index-start;

  ln.chars.index := ln.chars.TextLength; //<== if we get here we know there is an unterminated string on the line
end;


function TScanner.AllowableWordChar(Const c : char) : boolean;
begin
  result := ord(c) in [ord(lowercase_a)..ord(lowercase_z),ord(uppercase_a)..ord(uppercase_z),ord(underscore)];
end;

function TScanner.MatchSpecialChar(const c : char) : boolean;
begin
  result := ord(c) in [
    ord(Quotes),
    ord(open_bracket),
    ord(Close_Bracket),
    ord(Asterisk),
    ord(Plus),
    ord(Comma),
    ord(minus),
    ord(dot),
    ord(Bang),
    ord(Equal),
    ord(Less_than),
    ord(greater_than)];
end;

function TScanner.MakeWordToken: TToken;
var
  CurrentChar : Char;
begin
  result := nullToken;

  currentChar := ln.chars.current;  //grab the current char

  if not (allowableWordChar(currentChar)) then exit;  //check to see if it's allowed

  result.kind := tkIdentifier; //set the current result to a word
  result.start := ln.chars.index; //set the result current index to current char index;
  result.length := 1;
  result.line := ln.lineIndex;
  result.Text := currentChar;
  while (ln.chars.Next <> cNull) and allowableWordChar(ln.chars.current) do  //start looping over the next chars until we break at a non allowable char (i.e. @)
  begin
      inc(result.length);
      result.Text := Result.Text + ln.chars.current;
  end;

  if (result.length >= 1) and (MatchSpecialChar(ln.chars.current) or CurrentCharIsNumber) then
  begin
    ln.chars.Move(-1);
  end;
end;


function TScanner.GetReserverdWordToken(const TokenKind : TTokenKind) : TToken;
begin
  result := NullToken;

  if not MatchKeyWord(TTokenName[TokenKind]) then exit;
  if not checkKeyWord(TTokenName[TokenKind]) then exit;

  begin
    result.start := ln.chars.index;
    result.length := length(TTokenName[TokenKind]);
    result.line := ln.LineIndex;
    result.Kind := tokenKind;
    result.text := TTokenName[tokenKind];
  end;
end;


function TScanner.ReservedOrWord(tokenKind : TTokenKind) : TToken;
begin
  result := GetReserverdWordToken(tokenKind); //try and make a reserved word, 1st.  Get reserved word doesn't touch the index.
  if result.kind <> tkNull then
  begin
    //move the index to the end of the reserved word;
    //we need to move the index to
    //    a) end of the word if the last letter of the word is at the end of the line (i.e. no crlf)
    // OR b) next char

    if (result.start + result.length -1) <= LineLength  then //middle of line  12345AND12345678
    begin
      ln.chars.Move(result.length-1);
    end;
    exit;
  end;

  result := MakeWordToken;     //if we can't make a reserved word, try and complete a word, if this fails the index will be at the end of the line
end;

function TScanner.NullToken : TToken;
begin
  fillchar(result,sizeof(result),cNull);
  result.kind := tkNull;
end;


function TScanner.CurrentCharIsNumber : boolean;
begin
  result := charIsNumber(ln.chars.current);
end;

function TScanner.charIsNumber(const c : char) : boolean;
begin
  result := MatchAny(c,[zero,one,two,three,four,five,six,seven,eight,nine]);
end;

//123456.123456
function TScanner.MakeNumberToken : TToken;

  procedure ScanNumber;
  begin
    //increment until char is not a number (or null)
    while charIsNumber(ln.chars.next) do
    begin
      inc(result.length);
      result.text := result.text + ln.chars.current;
    end;
  end;

begin
  result := nullToken;
  if not currentCharIsNumber then exit;

  result.text := ln.chars.current;
  result.Kind := tkNumber;
  result.start:= ln.chars.index;
  result.line:= ln.lineindex;
  result.length :=1;


  ScanNumber;

  if (Match(dot)) and charIsNumber(ln.Chars.PeekNext) then //we sit on a '.' and the next char is 1234567890
  begin
    result.text := result.text + ln.chars.current;
    inc(result.length);
    ScanNumber;
  end;


  if not currentCharIsNumber then //it might be on the end of the line
  begin
    ln.chars.prev;
  end;
end;

function TScanner.MakeToken : TToken;
  var
    Token : TToken;
  begin
     token := nullToken;
     try
        if not CharInAsiiSubset then exit;

        //test for tokens which can be combined int 2 tokens, i.e. !=, <=, >=, ==
        case ord(ln.chars.Current) of

        ord(open_Bracket) : begin
          Token.Kind := tkOpen_Bracket;
        end;

        ord(close_Bracket) : begin
          Token.Kind := tkClose_Bracket;
        end;

        ord(Curly_Opening_brace) : begin
          Token.Kind := tkOpenBrace;
        end;

        ord(Curly_Closing_brace) : begin
          Token.Kind := tkCloseBrace;
        end;

        ord(Asterisk) : begin
          Token.Kind := tkAsterisk;
        end;

        ord(Plus) : begin
          Token.Kind := tkPlus;
        end;

        ord(Comma) : begin
          Token.Kind := tkComma;
        end;

        ord(minus) : begin
          Token.Kind := tkminus;
        end;

        ord(dot) : begin
          Token.Kind := tkdot;
        end;

       (* ord(underscore) : begin
          Token.Kind := underscore;
        end; *)

        //!,!=
        ord(bang) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token.Kind := tkBangEqual;
             ln.chars.Next;
           end
           else
           begin
             Token.Kind := tkBang;
           end;
        end;

        //=, ==
        ord(equal) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token.Kind := tkEqualEqual;
             ln.chars.Next;
           end
           else
           begin
             Token.Kind := tkEqual;
           end;
        end;

        ord(Less_than) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token.Kind := tkLessThanEqual;
             ln.chars.Next;
           end
           else
           begin
             Token.Kind := tkLess_than;
           end;
        end;

        ord(Greater_than) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token.Kind := tkGreaterThanEqual;
             ln.chars.Next;
           end
           else
           begin
             Token.Kind := tkgreater_than;
           end;
        end;

        ord(Zero),
        ord(One),
        ord(Two),
        ord(Three),
        ord(Four),
        ord(Five),
        ord(Six),
        ord(Seven),
        ord(Eight),
        ord(Nine) :
        begin
          Token := MakeNumberToken;
        end;



        // check for AND
        ord(lowercase_a), ord(uppercase_a) :
        begin
          Token := ReservedOrWord(tkAnd);
        end;

        ord(lowercase_c), ord(uppercase_c) :
        begin
          Token := ReservedOrWord(tkClass);

        end;

        ord(lowercase_e), ord(uppercase_e) :
        begin
           Token := ReservedOrWord(tkElse);
        end;

        //check false, fun, for
        ord(lowercase_f), ord(uppercase_f) :
        begin

          case ord(ln.chars.PeekNext) of
            ord(lowercase_a),ord(uppercase_a) :
            begin
              Token := ReservedOrWord(tkFalse);
            end;
            ord(lowercase_u), ord(uppercase_u) :
            begin
              Token := ReservedOrWord(tkFun);
            end
            else
            begin
               Token := ReservedOrWord(tkFor);
            end;
          end;
        end;

        ord(lowercase_i), ord(uppercase_i) :
        begin
          Token := ReservedOrWord(tkIf);
        end;
        ord(lowercase_n), ord(uppercase_n) :
        begin
          Token := ReservedOrWord(tkNil);
        end;

        ord(lowercase_o), ord(uppercase_o) :
        begin
           Token := ReservedOrWord(tkOr);
        end;

        ord(lowercase_p), ord(uppercase_p) :
        begin
           Token := ReservedOrWord(tkPrint);
        end;

         //string
        ord(quotes) : begin
           Token := MakeStringToken;
        end;

        ord(lowercase_r), ord(uppercase_r) :
        begin

           Token := ReservedOrWord(tkReturn);

        end;

        ord(lowercase_t), ord(uppercase_t) :
        begin
          Token := ReservedOrWord(tkTrue);
        end;

        ord(lowercase_v), ord(uppercase_v) :
        begin
           Token := ReservedOrWord(tkVar);
        end;

        ord(lowercase_w), ord(uppercase_w) :
        begin
           Token := ReservedOrWord(tkWhile);
        end;

        ord(space) : begin
            Token.Kind := tkWhiteSpace;
        end
        else
        begin //other letters not considered prefix of reserved words.
          Token := MakeWordToken;
        end;
     end;


     Token.line:=  ln.LineIndex;

     finally
       result := Token;
     end;

  end;

function TScanner.CharInAsiiSubset() : Boolean;
begin
  result := (ord(ln.chars.Current) >= ord(space)) and (ord(ln.chars.Current) <= ord(Delete));
end;



  function TScanner.ScanLine(const LineRecord : TLine) : TTokens;
  var
    Token : TToken;
  begin
    if ln.LineRecordIsNull(LineRecord) then exit;
    ln.chars.Init(LineRecord.text);
    if ln.chars.HasNext then
    begin
      while ln.chars.Next <> cNull do
      begin
        Token := MakeToken;

        if Token.Kind = tkWhiteSpace then
        begin
          //if SkipWhiteSpace then ln.chars.move(-1);
        end
        else
        if (Token.kind <> tkNull) then
        begin
          inc(TokenIndex);
          Tokens[TokenIndex] := Token;
          inc(TokenCount);
        end;
      end;
    end;
  end;

function TScanner.Match(const ascii : TAscii) : boolean;
begin
  result :=  MatchChar(ln.chars.Current,ascii);
end;

function TScanner.SkipWhiteSpace : boolean;
begin
  result := false;
  if not Match(space) then exit;


  while MatchChar(ln.chars.PeekTo(ln.chars.index),space) do
  begin
    result := true;
    ln.chars.next;
  end;

end;

procedure TScanner.Scan;
var
  Line : TLine;
begin
  While ln.HasNext do
  begin
    Line := ln.Next;
    ScanLine(Line);
  end;
end;


end.





