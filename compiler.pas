unit compiler;

interface

uses LOXTypes, TokenArray;


type

 TParseFn = procedure(const canAssign : boolean) of object;

 TPrecedence = (
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
 );

  TParseRule = record
    IsUsed : Boolean;
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkthis] of TParseRule;

  TCompiler = class
  private
    FCurrent : pToken;
    FParseRules : TParseRules;
    FTokens : TTokenIterator;
    procedure CreateRuleForOpen_bracket;
    procedure CreateRulesForClose_bracket;
    procedure CreateRules;
    procedure init;
    procedure Error(Const msg : String);
    function argumentList: Byte;
    function match(const TokenKind : TTokenKind) : boolean;
    procedure EmitBytes(const Byte1,Byte2 : Byte);
  public
    procedure parsePrecedence(precedence : TPrecedence);
    function advance : boolean;
    procedure grouping(const canAssign : boolean);
    function getRule(TokenKind : TTokenKind) : TParseRule;
    procedure consume(const Token : TTokenKind; const Message : String);
    procedure expression;
    procedure call(const canAssign : boolean);
    constructor Create(Const Tokens : TTokenIterator);
  end;





implementation

function TCompiler.getRule(TokenKind : TTokenKind) : TParseRule;
begin
  result :=  FParseRules[tokenKind];
end;


function TCompiler.advance : boolean;
begin
  FCurrent := FTokens.Next;
  result := FCurrent <> nil;
end;


procedure TCompiler.parsePrecedence(precedence : TPrecedence);
var
  prefixRule : TParseFn;
  infixRule : TParseFn;
  canAssign : boolean;
begin
  advance; //move to the next token. This can and will return nil at some points, therefore the code below would explode, currently

  prefixRule := getRule(FTokens.prev.kind).prefix;
  if (@prefixRule = nil) then
  begin
    error('Expected expression.');
    exit;
  end;

  canAssign := precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(FTokens.current.Kind).precedence) do
  begin
    advance;
    infixRule := getRule(FTokens.prev.Kind).infix;
    infixRule(canAssign);
  end;

  if canAssign then 
  begin
    if FTokens.Current.Kind <> tkEqual then
    begin
      error('Invalid assignment target.');
      exit;
    end;
    advance;
  end;
end;

procedure TCompiler.expression;
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure TCompiler.consume(const Token : TTokenKind; const Message : String);
begin

end;

constructor TCompiler.Create(Const Tokens : TTokenIterator);
begin
  FTokens := Tokens;
end;

procedure TCompiler.CreateRuleForOpen_bracket;
begin
  FParseRules[tkopen_bracket].Prefix := grouping;
  FParseRules[tkopen_bracket].Infix := call;
  FParseRules[tkopen_bracket].Precedence := PREC_CALL;
end;


procedure TCompiler.CreateRulesForClose_bracket;
begin
  // [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  FParseRules[tkclose_bracket].Prefix := nil;
  FParseRules[tkclose_bracket].Infix := nil;
  FParseRules[tkclose_bracket].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRules;
begin
  CreateRuleForOpen_bracket;
  CreateRulesForClose_bracket;
end;

procedure TCompiler.Error(const msg: String);
begin
  //not sure what to do here right now.
end;

procedure TCompiler.grouping(const canAssign : boolean);
begin
   expression();
   consume(tkopen_bracket, 'Expect '')'' after expression.');
end;

procedure TCompiler.init;
begin
  FillChar(FParseRules,sizeof(FParseRules),#0);
  CreateRules;
end;

function TCompiler.match(const TokenKind : TTokenKind) : boolean;
begin
  result := false;
  if not (FTokens.Current.Kind = TokenKind) then
  begin
    exit;
  end;

  advance();
end;

function TCompiler.argumentList: Byte;
var
  argCount: Byte;
begin
  argCount := 0;
  if not (FTokens.Current.Kind = tkclose_Bracket) then  //check(TOKEN_RIGHT_PAREN) then
  begin
    repeat   //looking at the c code is this equivalent looping?
      expression;
      if argCount = 255 then
      begin
        error('Can''t have more than 255 arguments.');
      end;
      Inc(argCount);
    until not match(tkComma);
  end;
  consume(tkOpen_Bracket, 'Expect '')'' after arguments.');
  Result := argCount;
end;

(*
static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
//> arg-limit
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
//< arg-limit
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}   *)


(*
static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}
//< Compiling Expressions emit-byte
//> Compiling Expressions emit-bytes
static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
} *)


procedure TCompiler.EmitBytes(const Byte1,Byte2 : Byte);
begin
   //emitByte(
end;


procedure TCompiler.call(const canAssign : boolean);
var
  argCount : byte;
begin
  argCount := argumentList();
  emitBytes(ord(OP_CALL), argCount);
end;





end.

 (*  this is really cool, until you try to assign functions from within records, or classes. It doesn't like it at all. There's probably a super dodgy way to do it with a pointer. but alas, it doesn't matter much to me
  const

  TParseRules: array[tknull..tkthis] of TParseRule = (
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tknull
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkNumber
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkQuotes
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkUnterminatedQuotes
    (prefix: grouping; infix: call; precedence: PREC_CALL), //tkopen_bracket
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkclose_Bracket
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkAsterisk
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkPlus
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkComma
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkminus
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkBangEqual
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkBang
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkEqual
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkEqualEqual
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkLessThanEqual
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkLess_than
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkGreaterThanEqual
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkgreater_than
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkAnd
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkClass
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkElse
    (prefix: nil; infix: nil; precedence: PREC_NONE), //tkFalse
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkFun
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkFor
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkIf
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkNil
    (prefix: nil; infix: nil; precedence: PREC_NONE),// tkOr
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkPrint
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkReturn
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkTrue
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkVar
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkWhile
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkIdentifier
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkWhiteSpace
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkOpenBrace
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkCloseBrace
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkInteger
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkunderscore
    (prefix: nil; infix: nil; precedence: PREC_NONE),//tkThis
    (prefix: nil; infix: nil; precedence: PREC_NONE));    *)





