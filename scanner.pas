unit scanner;

interface
uses
  sysutils,LoxTypes, charIterator,LineIterator,TokenArray;

type


 
  TScanner = record
    TokenIndex : integer;
    Tokens : TTokens;
    TokenCount : integer;
    ln : TLineIterator;
    function MakeToken : TToken;

    function MakeWordToken : TToken;
    function MakeStringToken : TToken;
    function MakeEOFToken : TToken;
    function MakeBangEqualToken : TToken;
    function MakeCommentToken : TToken;
    function MakeGreaterThanOrEqualToToken : TToken;
    function MakeLessThanOrEqualToken : TToken;
    function MakeEqualEqualToken : TToken;
    function MakeSingleToken(const Kind : TTokenKind) : TToken;
    function MakeNumberToken : TToken;
    function CurrentCharIsNumber : boolean;
    function charIsNumber(const c : char) : boolean;
    function MatchAny(const c : char; const Any : Array of TAscii) : boolean;
    function MatchChar(const c : char; const ascii : TAscii) : boolean;
    function Match(const Ascii : TAscii) : boolean;

    function NullToken : TToken;
    function GetReserverdWordToken(const TokenKind : TTokenKind) : TToken;
    function MakeReservedOrNormalWordToken(tokenKind : TTokenKind) : TToken;
    function AllowableWordChar(Const c : char) : boolean;
    function MatchSpecialChar(const c : char) : boolean;

    function SkipWhiteSpace : boolean;

    function LineLength : Integer;
    function SpaceBeforeWordStart(const WordStart : integer) : boolean;
    function SpaceAfterWordEnd(const WordEnd : integer) : boolean;
    function OpenBracketAfterWordEnd(const WordEnd : integer) : boolean;
    function MatchKeyWord(const word : String) : boolean;
    function CheckKeyWord(const word : String) : boolean;

    function CharInAsiiSubset : Boolean;
    function ScanLine(
      const LineRecord : TLine) : TTokens;
    procedure Init(const text : string);

    function TokenText(const Token : TToken) : String;

    procedure finalize;
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
    tokens := TTokens.Create;
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

function TScanner.TokenText(const Token: TToken): String;
begin
  result :=  copy(ln.items[Token.Line].text,token.Start,token.length);
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
  result := nil;

  if not Match(Quotes) then exit;

  start := ln.chars.index; idx := start;

  //result.text := ln.chars.current;
  while (ln.chars.next <> cNull) and (Match(quotes) = false) do
  begin
    //result.text := result.text + ln.chars.current;
    inc(idx);
  end;

  if Match(Quotes) then
  begin
     //result.text := result.text + ln.chars.current;
     result := TToken.Create;
     result.kind := tkQuotes;
     result.start := start + 1;
     result.length := (idx+1)-result.Start;

     exit;  //<=== exit here
  end;

  //unterminated string, i.e. the current char we're on is not a "
  //result.text := result.text + ln.chars.current;
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
    ord(semicolon),
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
  result := nil;

  currentChar := ln.chars.current;  //grab the current char

  if not (allowableWordChar(currentChar)) then exit;  //check to see if it's allowed

  result := TToken.create;
  result.kind := tkIdentifier; //set the current result to a word
  result.start := ln.chars.index; //set the result current index to current char index;
  result.length := 1;
  result.line := ln.lineIndex;
//  result.Text := currentChar;
  while (ln.chars.Next <> cNull) and allowableWordChar(ln.chars.current) do  //start looping over the next chars until we break at a non allowable char (i.e. @)
  begin
      inc(result.length);
      //result.Text := Result.Text + ln.chars.current;
  end;

  result.txt := copy(ln.chars.text,result.start,result.length);

  if (result.length >= 1) and (MatchSpecialChar(ln.chars.current) or CurrentCharIsNumber ) then
  begin
    ln.chars.Move(-1);
  end;

  (*text := FScanner.ln.items[Token.Line].text;
  text := copy(text,token.Start,token.length);
*)

  //result.txt := copy(ln.text,result.start,result.length);
end;


function TScanner.GetReserverdWordToken(const TokenKind : TTokenKind) : TToken;
begin
  result := nil;

  if not MatchKeyWord(TTokenName[TokenKind]) then exit;
  //if not checkKeyWord(TTokenName[TokenKind]) then exit;   I think this was my initial attempt at checking the text for errors.
  // but it doesn't quite work correctly. i.e. (true==false) will not pass the test, therefore I have removed it temporarily.

    result := TToken.create;
    result.start := ln.chars.index;
    result.length := length(TTokenName[TokenKind]);
    result.line := ln.LineIndex;
    result.Kind := tokenKind;


end;


function TScanner.MakeReservedOrNormalWordToken(tokenKind : TTokenKind) : TToken;
begin
  result := GetReserverdWordToken(tokenKind); //try and make a reserved word, 1st.  Get reserved word doesn't touch the index.
  if result <> nil then
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

procedure TScanner.finalize;
begin
  tokens.free;
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
      //result.text := result.text + ln.chars.current;
    end;
  end;

begin
  result := nil;
  if not currentCharIsNumber then exit;

  //result.text := ln.chars.current;
  result := TToken.Create;
  result.Kind := tkNumber;
  result.start:= ln.chars.index;
  result.line:= ln.lineindex;
  result.length :=1;


  ScanNumber;

  if (Match(dot)) and charIsNumber(ln.Chars.PeekNext) then //we sit on a '.' and the next char is 1234567890
  begin
    //result.text := result.text + ln.chars.current;
    inc(result.length);
    ScanNumber;
  end;


  if not currentCharIsNumber then //it might be on the end of the line
  begin
    ln.chars.prev;
  end;
end;

function TScanner.MakeEOFToken;
begin
   result := TToken.Create;
   result.Kind := TkEOF;
   result.start:= ln.chars.TextLength+1;  //because whatever the current length is we will add this onto it.
   result.length:= 1;
   result.line:= ln.lineIndex;
end;

function TScanner.MakeSingleToken(const Kind : TTokenKind) : TToken;
begin
   result := TToken.Create;
   result.Kind := Kind;
   result.start:= ln.chars.index;
   result.length:= 1;
   result.line:= ln.lineIndex;
   //result.text := TTokenName[Kind];
end;

function TScanner.MakeGreaterThanOrEqualToToken : TToken;
begin
  result := TToken.Create;
  result.Kind := tkGreaterThanEqual;
  result.start:= ln.chars.index;
  result.length:= 2;
  result.line:= ln.lineIndex;
  //result.text := TTokenName[tkGreaterThanEqual];
end;

function TScanner.MakeLessThanOrEqualToken : TToken;
begin
  result := TToken.Create;
  result.Kind := tkLessThanEqual;
  result.start:= ln.chars.index;
  result.length:= 2;
  result.line:= ln.lineIndex;
  //result.text := TTokenName[tkLessThanEqual];
end;

function TScanner.MakeEqualEqualToken : TToken;
begin
  result := TToken.Create;
  result.Kind := tkEqualEqual;
  result.start:= ln.chars.index;
  result.length:= 2;
  result.line:= ln.lineIndex;
  //result.text := TTokenName[tkEqualEqual];
end;

function TScanner.MakeBangEqualToken : TToken;
begin
  result := TToken.Create;
  result.Kind := tkBangEqual;
  result.start:= ln.chars.index;
  result.length:= 2;
  result.line:= ln.lineIndex;
  //result.text := TTokenName[tkBangEqual];
end;


function TScanner.MakeCommentToken: TToken;
begin
  result := TToken.Create;
  result.Kind := tkComment;
  result.Start := ln.Chars.Index;
  result.Length := ln.Chars.TextLength-result.start+1;
  ln.Chars.Last;
end;

function TScanner.MakeToken : TToken;
  var
    Token : TToken;
  begin

     token := nil;
     try
        if not CharInAsiiSubset then exit;

        //test for tokens which can be combined int 2 tokens, i.e. !=, <=, >=, ==
        case ord(ln.chars.Current) of

        ord(open_Bracket) : begin
          Token := MakeSingleToken(tkOpenBracket)
        end;

        ord(close_Bracket) : begin
          Token := MakeSingleToken(tkCloseBracket);
        end;

        ord(Curly_Opening_brace) : begin
          Token := MakeSingleToken(tkOpenBrace);
        end;

        ord(Curly_Closing_brace) : begin
          Token := MakeSingleToken(tkCloseBrace);
        end;

        ord(Asterisk) : begin
          Token := MakeSingleToken(tkAsterisk);
        end;

        ord(Plus) : begin
          Token := MakeSingleToken(tkPlus);
        end;

        ord(Comma) : begin
          Token := MakeSingleToken(tkComma);
        end;

        ord(minus) : begin
          Token := MakeSingleToken(tkminus);
        end;

        ord(dot) : begin
          Token := MakeSingleToken(tkdot);
        end;

        ord(semiColon) : begin
           Token := MakeSingleToken(tkSemiColon);
        end;

        ord(slash) : begin
           if MatchChar(ln.chars.PeekNext,slash) then
           begin
             Token := MakeCommentToken;
           end
           else
             Token := MakeSingleToken(tkSlash);
        end;

        ord(underscore) : begin
          //Token.Kind := tkunderscore;
        end;



        //!,!=
        ord(bang) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token:= MakeBangEqualToken;
             ln.chars.Next;
           end
           else
           begin
             Token := MakeSingleToken(tkBang);
           end;
        end;




        //=, ==
        ord(equal) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token  := MakeEqualEqualToken;
             ln.chars.Next;
           end
           else
           begin
             Token := MakeSingleToken(tkEqual);
           end;
        end;

        ord(Less_than) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token := MakeLessThanOrEqualToken;
             ln.chars.Next;
           end
           else
           begin
             Token := MakeSingleToken(tkLessthan);
           end;
        end;

        ord(Greater_than) : begin
           if MatchChar(ln.chars.PeekNext,equal) then
           begin
             Token := MakeGreaterThanOrEqualToToken;
             ln.chars.Next;
           end
           else
           begin
             Token := MakeSingleToken(tkgreaterthan);
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
          Token := MakeReservedOrNormalWordToken(tkAnd);
        end;

        ord(lowercase_c), ord(uppercase_c) :
        begin
          Token := MakeReservedOrNormalWordToken(tkClass);

        end;

        ord(lowercase_e), ord(uppercase_e) :
        begin
           Token := MakeReservedOrNormalWordToken(tkElse);
        end;

        //check false, fun, for
        ord(lowercase_f), ord(uppercase_f) :
        begin

          case ord(ln.chars.PeekNext) of
            ord(lowercase_a),ord(uppercase_a) :
            begin
              Token := MakeReservedOrNormalWordToken(tkFalse);
            end;
            ord(lowercase_u), ord(uppercase_u) :
            begin
              Token := MakeReservedOrNormalWordToken(tkFun);
            end
            else
            begin
               Token := MakeReservedOrNormalWordToken(tkFor);
            end;
          end;
        end;

        ord(lowercase_i), ord(uppercase_i) :
        begin
          Token := MakeReservedOrNormalWordToken(tkIf);
        end;
        ord(lowercase_n), ord(uppercase_n) :
        begin
          Token := MakeReservedOrNormalWordToken(tkNil);
        end;

        ord(lowercase_o), ord(uppercase_o) :
        begin
           Token := MakeReservedOrNormalWordToken(tkOr);
        end;

        ord(lowercase_p), ord(uppercase_p) :
        begin
           Token := MakeReservedOrNormalWordToken(tkPrint);
        end;

         //string
        ord(quotes) : begin
           Token := MakeStringToken;
        end;

        ord(lowercase_r), ord(uppercase_r) :
        begin

           Token := MakeReservedOrNormalWordToken(tkReturn);

        end;




        ord(lowercase_t), ord(uppercase_t) :
        begin
           case ord(ln.chars.PeekNext) of
            ord(lowercase_h),ord(uppercase_h) : Token := MakeReservedOrNormalWordToken(tkThis);
            ord(lowercase_r),ord(uppercase_r) : Token := MakeReservedOrNormalWordToken(tkTrue);

           end;
        end;

        ord(lowercase_v), ord(uppercase_v) :
        begin
           Token := MakeReservedOrNormalWordToken(tkVar);
        end;

        ord(lowercase_w), ord(uppercase_w) :
        begin
           Token := MakeReservedOrNormalWordToken(tkWhile);
        end;

        ord(space) : begin
            Token := TToken.create;
            Token.Kind := tkWhiteSpace;
        end
        else
        begin //other letters not considered prefix of reserved words.
          Token := MakeWordToken;
        end;
     end;

     if Token <> nil then
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

    while ln.chars.Next <> cNull do
    begin
      Token := MakeToken;
      if Token <> nil then
      begin

        if Token.Kind = tkWhiteSpace then
        begin
          //if SkipWhiteSpace then ln.chars.move(-1);
        end
        else
        if (Token.kind <> tkNull) then
        begin
          inc(TokenIndex);
//          Tokens[TokenIndex] := Token;
          Tokens.add(Token);
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
  Token : TToken;
begin
  while ln.HasNext do
  begin
    Line := ln.Next;
    ScanLine(Line);
  end;
  //finish of the tokens with an EOF token. This is important later when compiling.
  Token := makeEOFToken;
  Tokens.add(Token);
  inc(TokenCount);
end;


end.






