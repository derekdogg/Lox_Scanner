unit NewScanner;

interface

uses
  sysutils, classes, loxTypes, TokenArray;

const
  Token_Capacity = 256;
  Null = 0;
  Space = 32;
  Bang = 33;
  Quotes = 34;
  Hash = 35;
  Dollar = 36;
  Percent = 37;
  Ampersand = 38;
  Single_quote = 39;
  Open_Bracket = 40;
  Close_bracket = 41;
  Asterisk = 42;
  Plus = 43;
  Comma = 44;
  Minus = 45;
  Dot = 46;
  Slash = 47;
  Zero = 48;
  One = 49;
  Two = 50;
  Three = 51;
  Four = 52;
  Five = 53;
  Six = 54;
  Seven = 55;
  Eight = 56;
  Nine = 57;
  Colon = 58;
  Semicolon = 59;
  Less_than = 60;
  Equal = 61;
  Greater_than = 62;
  Question_mark = 63;
  At_sign = 64;
  Uppercase_A = 65;
  Uppercase_B = 66;
  Uppercase_C = 67;
  Uppercase_D = 68;
  Uppercase_E = 69;
  Uppercase_F = 70;
  Uppercase_G = 71;
  Uppercase_H = 72;
  Uppercase_I = 73;
  Uppercase_J = 74;
  Uppercase_K = 75;
  Uppercase_L = 76;
  Uppercase_M = 77;
  Uppercase_N = 78;
  Uppercase_O = 79;
  Uppercase_P = 80;
  Uppercase_Q = 81;
  Uppercase_R = 82;
  Uppercase_S = 83;
  Uppercase_T = 84;
  Uppercase_U = 85;
  Uppercase_V = 86;
  Uppercase_W = 87;
  Uppercase_X = 88;
  Uppercase_Y = 89;
  Uppercase_Z = 90;
  Open_Square_Bracket = 91;
  Backslash = 92;
  Close_Square_Bracket = 93;
  Caret = 94;
  Underscore = 95;
  Grave_Accent = 96;
  Lowercase_a = 97;
  Lowercase_b = 98;
  Lowercase_c = 99;
  Lowercase_d = 100;
  Lowercase_e = 101;
  Lowercase_f = 102;
  Lowercase_g = 103;
  Lowercase_h = 104;
  Lowercase_i = 105;
  Lowercase_j = 106;
  Lowercase_k = 107;
  Lowercase_l = 108;
  Lowercase_m = 109;
  Lowercase_n = 110;
  Lowercase_o = 111;
  Lowercase_p = 112;
  Lowercase_q = 113;
  Lowercase_r = 114;
  Lowercase_s = 115;
  Lowercase_t = 116;
  Lowercase_u = 117;
  Lowercase_v = 118;
  Lowercase_w = 119;
  Lowercase_x = 120;
  Lowercase_y = 121;
  Lowercase_z = 122;
  Curly_Opening_brace = 123;
  Vertical_bar = 124;
  Curly_Closing_brace = 125;
  Tilde = 126;
  Delete = 127;

type

  TErrorNumber = 0..Token_Capacity-1;

   



  TTokenizer = record
    Capacity : integer;
    Tokens : TTokens;
    TokenCount : integer;
    LineNo : integer;
    Line   : String;
    LineLength   : integer;
    CharIndex   : integer;
    //CurrentLine : integer;
    Lines  : TStrings;
    function IsReservedWord(const word : string) : TTokenKind;
    function MakeEOFToken(const Kind : TTokenKind) : TToken;
    function MakeNumberToken : TToken;
    function MakeEqualEqualToken : TToken;
    function MakeSingleCharToken(const Kind : TTokenKind) : TToken;
    function MakeIdentifier : TToken;
    function MakeString : TToken;
    function PeekNext : integer;
    procedure AddToken(const token : TToken);

    function  AllowableWordChars(Const c : char) : boolean;
    function  AllowableNumberChars(Const c : char) : boolean;
    procedure ParseLine;
    procedure parse(const text : string);
    procedure Reset;
  end;







implementation
uses strUtils,dialogs;

{ TTokenizer }

function TTokenizer.AllowableWordChars(Const c : char) : boolean;
begin
  result := ord(c) in [
      Zero..Nine,
      lowercase_a..lowercase_z,
      uppercase_a..uppercase_z,
      underscore];
end;

function TTokenizer.AllowableNumberChars(Const c : char) : boolean;
begin
  result := ord(c) in [Zero..Nine, dot];
end;
 


procedure TTokenizer.Reset;
begin
  LineNo := 1; 
  TokenCount := 0;
  Capacity := Token_Capacity;
  SetLength(Tokens,Capacity);

end;

function TTokenizer.MakeString: TToken;
var
  continue : boolean;
begin
  result.text := '';
  result.Kind := tkQuotes;
  result.Line := LineNo;
  result.pos  := CharIndex;
  Assert(CharIndex <= LineLength, 'Current char index is > length of line');

  continue := true;

  while continue and (CharIndex <= LineLength) do
  begin
    result.text := result.text + Line[CharIndex];
    inc(CharIndex);
    continue := ord(Line[CharIndex]) <> quotes;
  end;

  if ord(Line[CharIndex]) = quotes then
  begin
    result.text := result.text + '"';
    inc(CharIndex);
  end;
end;
 

procedure TTokenizer.AddToken(const token: TToken);
begin
  if TokenCount = Capacity then
  begin
    Capacity := Capacity * 2;
    SetLength(Tokens,Capacity);
  end;
  Tokens[TokenCount] := token;
  inc(TokenCount);
end;


function TTokenizer.IsReservedWord(const word : string) : TTokenKind;
var
  Kind : TTokenKind;

begin
  result := tkNull;
  for Kind := tkAnd to tkWhile do
  begin
    if lowercase(word) = lowercase(TReservedWords[Kind]) then
    begin
      result := Kind;
      break;
    end;
  end;
end;

function TTokenizer.MakeIdentifier : TToken;
var
  text : string;
  continue : boolean;
  reservedWord : TTokenKind;
begin
  text := '';
  Assert(CharIndex <= LineLength, 'Current char index is > length of line');

  continue := true;
  result.Line := LineNo;
  result.pos  := CharIndex;
  while continue and (CharIndex <= LineLength) do
  begin
    continue := allowableWordChars(Line[CharIndex]);
    if continue then
    begin
      text := text + Line[CharIndex];
      inc(CharIndex);
    end;
  end;

  result.Text := Text;

  reservedWord := IsReservedWord(text);
  if reservedWord <> tkNull then
  begin
    result.Kind := ReservedWord;
  end
  else
    result.Kind := tkIdentifier;

end;


function TTokenizer.MakeEOFToken(const Kind : TTokenKind) : TToken;
begin
  result.Text := 'EOF';
  result.Line := LineNo;
  result.pos  := CharIndex + 1;
  result.Kind := Kind;
end;


function TTokenizer.MakeSingleCharToken(const Kind : TTokenKind) : TToken;
begin
  result.Text := Line[CharIndex];
  result.Line := LineNo;
  result.pos  := CharIndex;
  result.Kind := Kind;
end;

function TTokenizer.MakeEqualEqualToken : TToken;
begin
  result.Text := '==';
  result.Line := LineNo;
  result.pos  := CharIndex;
  result.Kind := tkEqualEqual;
end;


function TTokenizer.MakeNumberToken : TToken;
var

  continue      : boolean;
  decimalCount : integer;
begin
  decimalCount := 0;

  Assert(CharIndex <= LineLength, 'Current char index is > length of line');

  result.Text := '';
  result.Line := LineNo;
  result.pos  := CharIndex;
  result.Kind := tkNumber;

  continue := true;

  while continue and (CharIndex <= LineLength) and (decimalCount <= 1) do
  begin
    continue := (AllowableNumberChars(Line[CharIndex]));
    if continue then
    begin
      result.text := result.text + Line[CharIndex];
      if ord(Line[CharIndex]) = dot then inc(DecimalCount);
      inc(CharIndex);
    end;
  end;

  if ord(result.Text[Length(Result.Text)]) = dot then
  begin
    result.Text := copy(result.Text,1,Length(Result.Text)-1);
  end;
end;


procedure TTokenizer.ParseLine ;
var
  Token : TToken;
begin
  assert(LineNo < Lines.Count, 'Index is beyond line count');
  assert(LineNo >=0 , 'Index is beyond line count');
  Line := Lines[LineNo];
  LineLength := Length(Line);
  if LineLength = 0 then exit;

  CharIndex := 1;
  while (CharIndex <= LineLength) do
  begin
    case Ord(Line[CharIndex]) of

      // Null
      Null: begin
        inc(CharIndex);
      end;

      // Space
      Space: begin
        inc(CharIndex);
      end;

      // Bang
      Bang: begin
        Token := MakeSingleCharToken(tkBang);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Quotes
      Quotes: begin
        Token := MakeString;
        AddToken(Token);
      end;

      // Hash
      Hash: begin
        Token := MakeSingleCharToken(tkHash);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Dollar
      Dollar: begin
        Token := MakeSingleCharToken(tkDollar);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Percent
      Percent: begin
        Token := MakeSingleCharToken(tkPercent);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Ampersand
      Ampersand:
      begin
        Token := MakeSingleCharToken(tkAmpersand);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Single quote
      Single_quote:
      begin
        Token := MakeSingleCharToken(tkSingleQuote);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Open Bracket
      Open_Bracket:
      begin
        Token := MakeSingleCharToken(tkOpenBracket);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Close Bracket
      Close_Bracket: begin
        Token := MakeSingleCharToken(tkCloseBracket);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Asterisk
      Asterisk: begin
        Token := MakeSingleCharToken(tkAsterisk);
        AddToken(Token);
        inc(CharIndex);

      end;

      // Plus
      Plus: begin
        Token := MakeSingleCharToken(tkPlus);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Comma
      Comma: begin
        Token := MakeSingleCharToken(tkComma);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Minus
      Minus: begin
        Token := MakeSingleCharToken(tkMinus);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Dot
      Dot: begin
        Token := MakeSingleCharToken(tkDot);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Slash
      Slash: begin
        Token := MakeSingleCharToken(tkSlash);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Digits 0-9
      Zero..Nine: begin
        Token := MakeNumberToken ;
        AddToken(Token);
      end;

      // Colon
      Colon: begin
        Token := MakeSingleCharToken(tkColon);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Semicolon
      Semicolon: begin
        Token := MakeSingleCharToken(tkSemiColon);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Less than
      Less_than: begin
        Token := MakeSingleCharToken(tkLessThan);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Equal
      Equal: begin
        if peekNext = Equal then
        begin
          Token := MakeEqualEqualToken;
          AddToken(Token);
          inc(CharIndex,2);
        end
        else
        begin
          Token := MakeSingleCharToken(tkEqual);
          AddToken(Token);
          inc(CharIndex);
        end;
      end;

      // Greater than
      Greater_than: begin
        Token := MakeSingleCharToken(tkGreaterThan);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Question mark
      Question_mark: begin
        Token := MakeSingleCharToken(tkQuestionMark);
        AddToken(Token);
        inc(CharIndex);
      end;

      // At sign
      At_sign: begin
        Token := MakeSingleCharToken(tkAtSign);
        AddToken(Token);
        inc(CharIndex);
      end;

      // _ a..z  A..Z
      Underscore,Lowercase_a..Lowercase_z,
      Uppercase_A..Uppercase_Z:
      begin
        Token := MakeIdentifier;
        AddToken(Token);
      end;

      // Open Square Bracket
      Open_Square_Bracket: begin
        Token := MakeSingleCharToken(tkOpenSquareBracket);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Backslash
      Backslash: begin
        Token := MakeSingleCharToken(tkBackSlash);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Close Square Bracket
      Close_Square_Bracket: begin
        Token := MakeSingleCharToken(tkCloseSquareBracket);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Caret
      Caret: begin
        Token := MakeSingleCharToken(tkCaret);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Grave Accent
      Grave_Accent: begin
        Token := MakeSingleCharToken(tkGraveAccent);
        AddToken(Token);
        inc(CharIndex);
      end;
 
      // Curly Opening Brace
      Curly_Opening_brace: begin
        Token := MakeSingleCharToken(tkOpenBrace);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Vertical Bar
      Vertical_bar: begin
       Token := MakeSingleCharToken(tkVerticalBar);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Curly Closing Brace
      Curly_Closing_brace: begin
        Token := MakeSingleCharToken(tkCloseBrace);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Tilde
      Tilde: begin
        Token := MakeSingleCharToken(tkTilde);
        AddToken(Token);
        inc(CharIndex);
      end;

      // Delete
      Delete:
      begin
        Token := MakeSingleCharToken(tkDelete);
        AddToken(Token);
        inc(CharIndex);
      end
      else
      begin
        Token := MakeSingleCharToken(tkMystery);
        AddToken(Token);
        inc(CharIndex);
      end;
    end;
  end;
end;

function TTokenizer.PeekNext: integer;
begin
  result := Null;
  if CharIndex < LineLength then
  begin
    result := ord(Line[CharIndex+1]);
  end;
end;

procedure TTokenizer.parse(const text : string);
var
  i : integer;
  EOF : TToken;
begin
  Lines := TStringList.create;
  Lines.Text := text;
  try
    reset;
    for i := 0 to Lines.Count-1 do
    begin
      LineNo := i;
      try
        parseLine;
      except
        on E:Exception do
        begin
          Showmessage(e.Message);
        end;
      end;
    end;
    EOF := MakeEOFToken(tkEOF);
    AddToken(EOF);
  finally
    Lines.Free;
  end;
end;








end.
