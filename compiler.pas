unit compiler;

interface

uses LOXTypes, TokenArray, Scanner, Chunk;


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
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkEOF] of TParseRule;

  TCompiler = class
  private
    FChunks : TChunks;
    FParseRules : TParseRules;
    FTokens : TTokenIterator;
    FScanner : TScanner;
//    procedure emitConstant(value : TValue);
 //   function NumberVal(num: Integer): TValue;
 //   function NumToValue(num: Double): TValue;
    function  match(const Expected : TTokenKind) : boolean;
    //------------------rules---------------------------------------------------
    procedure CreateRuleForOpen_bracket;
    procedure CreateRulesForClose_bracket;
    procedure CreateRulesForNumber;
    procedure CreateRulesForEqual;
    procedure CreateRulesForMultiply;
    procedure CreateRulesForEOF;
    procedure CreateRulesForDivide;
    procedure CreateRules;
    //--------------------------------------------------------------------------
    procedure init;
    procedure Error(Const msg : String);
    function argumentList: Byte;
    procedure CreateRulesForPlus;
    procedure CreateRulesForMinus;
    function advance : boolean;
    procedure EmitBytes(const Byte1,Byte2 : Byte);
    procedure Number(const canAssign : boolean);
    procedure Binary(const canAssign : boolean);
    procedure Unary(const canAssign : boolean);
    procedure call(const canAssign : boolean);
    procedure grouping(const canAssign : boolean);
    procedure parsePrecedence(precedence : TPrecedence);
    function getRule(TokenKind : TTokenKind) : TParseRule;
    procedure consume(const TokenKind : TTokenKind; const Message : String);
  public
    function Chunks : TChunks;
    procedure expression;
    constructor Create(Const Scanner : TScanner);
    destructor destroy;override;
  end;


implementation

uses sysutils;

function TCompiler.getRule(TokenKind : TTokenKind) : TParseRule;
begin
  result :=  FParseRules[tokenKind];
end;


function TCompiler.advance : boolean;
begin
  result := FTokens.MoveNext <> nil;
end;


(*  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }    *)



(*
  123+456

  previous current
  nil      123
  123      +

*)

procedure TCompiler.parsePrecedence(precedence : TPrecedence);
var
  prefixRule : TParseFn;
  infixRule : TParseFn;
  canAssign : boolean;
begin
  if not advance then exit;

  prefixRule := getRule(FTokens.Previous.kind).prefix;
  if (@prefixRule = nil) then
  begin
    error('Expected expression.');
    exit;
  end;

  canAssign := precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(FTokens.current.Kind).precedence) do
  begin
    if not advance then exit;
    infixRule := getRule(FTokens.previous.Kind).infix;
    if (@InfixRule = nil) then
    begin
      error('No infix rule. Expected.');
      exit;
    end;
    infixRule(canAssign);
  end;

  if (canAssign And match(tkEqual)) then
  begin
     error('Invalid assignment target.');
  end;
end;

procedure TCompiler.Unary(const canAssign: boolean);
var
  TokenKind : TTokenKind;
begin
  TokenKind := FTokens.previous.Kind;
  parsePrecedence(PREC_UNARY);
  case TokenKind of
    tkMinus : FChunks.AddNEGATE;

  end;


  //Emit the bytes now.
  (*  switch (operatorType) {
//> Types of Values compile-not
    case TOKEN_BANG: emitByte(OP_NOT); break;
//< Types of Values compile-not
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; // Unreachable.
  }
} *)
end;

procedure TCompiler.expression;
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;


(*
  //> Compiling Expressions consume
static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}
*)
procedure TCompiler.consume(const TokenKind : TTokenKind; const Message : String);
begin
  if FTokens.Current.Kind = TokenKind then
  begin
    Advance;
    Exit;
  end;
  error(Message)//errorAtCurrent(message);
end;

constructor TCompiler.Create(Const Scanner : TScanner);
begin
  FChunks.Init;
  FScanner := Scanner;

  FTokens.Init(Scanner.Tokens);
  FTokens.MoveFirst; //is this needed? I think you have to be sitting on 1st token, but at the moment, I can't determine entry point to this compiler in c code-base
  FillChar(FParseRules,sizeof(FParseRules),#0);
  CreateRules;
end;


procedure TCompiler.CreateRuleForOpen_bracket;
begin
  //   [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
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

procedure TCompiler.CreateRulesForDivide;
begin
//  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  FParseRules[tkSlash].Prefix := nil;
  FParseRules[tkSlash].Infix := binary;
  FParseRules[tkSlash].Precedence := PREC_FACTOR;
end;

procedure TCompiler.CreateRulesForNumber;
begin
  // [TOKEN_NUMBER]   = {number,   NULL,   PREC_NONE},
  FParseRules[tkNumber].Prefix := number;
  FParseRules[tkNumber].Infix := nil;
  FParseRules[tkNumber].Precedence := PREC_NONE;
end;


procedure TCompiler.CreateRulesForEqual;
begin
  // [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},

  FParseRules[tkEqual].Prefix := nil;
  FParseRules[tkEqual].Infix := nil;
  FParseRules[tkEqual].Precedence := PREC_NONE;
end;


procedure TCompiler.CreateRulesForPlus;
begin
  //[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},

  FParseRules[tkPlus].Prefix := nil;
  FParseRules[tkPlus].Infix := binary;
  FParseRules[tkPlus].Precedence := PREC_TERM;
end;



destructor TCompiler.destroy;
begin
  FChunks.Finalize;
  inherited;
end;

procedure TCompiler.CreateRulesForMinus;
begin
 // [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},

  FParseRules[tkMinus].Prefix := unary;
  FParseRules[tkMinus].Infix := binary;
  FParseRules[tkMinus].Precedence := PREC_TERM;
end;

procedure TCompiler.CreateRulesForMultiply;
begin
// [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},

  FParseRules[tkAsterisk].Prefix := nil;
  FParseRules[tkAsterisk].Infix := binary;
  FParseRules[tkAsterisk].Precedence := PREC_FACTOR;
end;

procedure TCompiler.CreateRulesForEOF;
begin
// [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},

  FParseRules[tkEOF].Prefix := nil;
  FParseRules[tkEOF].Infix := nil;
  FParseRules[tkEOF].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRules;
begin
  CreateRuleForOpen_bracket;
  CreateRulesForClose_bracket;
  CreateRulesForNumber;
  CreateRulesForEqual;
  CreateRulesForPlus;
  CreateRulesForMinus;
  CreateRulesForMultiply;
  CreateRulesForDivide;
  CreateRulesForEOF;
end;



procedure TCompiler.Error(const msg: String);
begin
  raise exception.create(msg);
 (* //not sure what to do here right now. C code included below...
  static void errorAt(Token* token, const char* message) {
//> check-panic-mode
  if (parser.panicMode) return;
//< check-panic-mode
//> set-panic-mode
  parser.panicMode = true;
//< set-panic-mode
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}
 *)
end;


(*function TCompiler.NumberVal(num: Integer): TValue;
begin
  Result := NumToValue(num);
end;

function TCompiler.NumToValue(num: Double): TValue;
var
  value: TValue;
begin
  Move(num, value, SizeOf(Double));
  Result := value;
end;    *)

(*
static void number(bool canAssign) {
//< Global Variables number
  double value = strtod(parser.previous.start, NULL);
/* Compiling Expressions number < Types of Values const-number-val
  emitConstant(value);
*/
//> Types of Values const-number-val
  emitConstant(NUMBER_VAL(value));
//< Types of Values const-number-val
} *)

(*procedure TCompiler.emitConstant(value : TValue);
begin {
  emitBytes(OP_CONSTANT, makeConstant(value));
}
  FChunks.AddConstant(Value);
end; *)

procedure TCompiler.Number(const canAssign : boolean);
var
  token : pToken;
  number : double;
  text : string;

begin
  Token := FTokens.Previous;
  if Token = nil then exit;

  text := FScanner.ln.items[Token.Line].text;
  text := copy(text,token.Start,token.length);

  number := strToFloat(text);

  FChunks.AddConstant(number);
end;

procedure TCompiler.grouping(const canAssign : boolean);
begin
   expression();
   consume(tkclose_bracket, 'Expect '')'' after expression.');  //right bracket
end;

procedure TCompiler.init;
begin

end;


function TCompiler.match(const Expected : TTokenKind) : boolean;
begin
  result := (FTokens.Current.Kind = Expected);
  if not result then exit;

  result := advance();
end;



function TCompiler.argumentList: Byte; //we never get to call currently with the limited expression we calculate...
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
  consume(tkclose_Bracket, 'Expect '')'' after arguments.');
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

function TCompiler.Chunks: TChunks;
begin
  result := FChunks;
end;

(*//< Global Variables binary
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1)); *)
procedure TCompiler.binary(const canAssign : boolean);
var
  TokenKind : TTokenKind;
  rule : TParseRule;
begin
  TokenKind := FTokens.Previous.Kind;
  rule := getRule(TokenKind);
  parsePrecedence(TPrecedence(ord(rule.Precedence) + 1));


  Case (TokenKind) of
    TkPlus      : FChunks.AddADD;
    TkMinus     : FChunks.AddSUBTRACT;
    tkAsterisk  : FChunks.AddMULTIPLY;
    tkEqual     : FChunks.AddEQUAL;
    tkSlash     : FChunks.AddDivide;
  end;

   {
//> Types of Values comparison-operators
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
//< Types of Values comparison-operators
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    default: return; // Unreachable.
  }

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
    (prefix: nil; infix: nil; precedence: PREC_NONE));



    ParseRule rules[] = {
/* Compiling Expressions rules < Calls and Functions infix-left-paren
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
*/
//> Calls and Functions infix-left-paren
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
//< Calls and Functions infix-left-paren
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, // [big]
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Classes and Instances table-dot
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
*/
//> Classes and Instances table-dot
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
//< Classes and Instances table-dot
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
/* Compiling Expressions rules < Types of Values table-not
  [TOKEN_BANG]          = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-not
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
//< Types of Values table-not
/* Compiling Expressions rules < Types of Values table-equal
  [TOKEN_BANG_EQUAL]    = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-equal
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
//< Types of Values table-equal
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-comparisons
  [TOKEN_EQUAL_EQUAL]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_GREATER]       = {NULL,     NULL,   PREC_NONE},
  [TOKEN_GREATER_EQUAL] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LESS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LESS_EQUAL]    = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-comparisons
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
//< Types of Values table-comparisons
/* Compiling Expressions rules < Global Variables table-identifier
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
*/
//> Global Variables table-identifier
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
//< Global Variables table-identifier
/* Compiling Expressions rules < Strings table-string
  [TOKEN_STRING]        = {NULL,     NULL,   PREC_NONE},
*/
//> Strings table-string
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
//< Strings table-string
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
/* Compiling Expressions rules < Jumping Back and Forth table-and
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
*/
//> Jumping Back and Forth table-and
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
//< Jumping Back and Forth table-and
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-false
  [TOKEN_FALSE]         = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-false
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
//< Types of Values table-false
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-nil
  [TOKEN_NIL]           = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-nil
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
//< Types of Values table-nil
/* Compiling Expressions rules < Jumping Back and Forth table-or
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
*/
//> Jumping Back and Forth table-or
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
//< Jumping Back and Forth table-or
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Superclasses table-super
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
*/
//> Superclasses table-super
  [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
//< Superclasses table-super
/* Compiling Expressions rules < Methods and Initializers table-this
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
*/
//> Methods and Initializers table-this
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
//< Methods and Initializers table-this
/* Compiling Expressions rules < Types of Values table-true
  [TOKEN_TRUE]          = {NULL,     NULL,   PREC_NONE},
*/
//> Types of Values table-true
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
//< Types of Values table-true
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};*)






