unit compiler;

interface

uses classes,LOXTypes, TokenArray, Scanner, Chunk, locals;


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
    FLogging : TStrings;
    FScopeDepth : integer;
//    FLocalCount : integer;
    Flocals : TLocalList;
    FChunks : TChunks;
    FParseRules : TParseRules;
    FTokens : TTokenIterator;
    FScanner : TScanner;
//    procedure emitConstant(value : TValue);
 //   function NumberVal(num: Integer): TValue;
 //   function NumToValue(num: Double): TValue;
    function TokenName(const token : pToken) : String;
    procedure Log(const txt : String);
    procedure Strings(const canAssign: Boolean);
    function  checkKind(Kind : TTokenKind) : boolean;
    function  match(const Expected : TTokenKind) : boolean;
    //------------------rules---------------------------------------------------
    procedure CreateRuleForOpen_bracket;
    procedure CreateRulesForClose_bracket;
    procedure CreateRulesForNumber;
    procedure CreateRulesForEqualEqual;
    procedure CreateRulesForNotEqual;
    procedure CreateRulesForMultiply;
    procedure CreateRulesForDivide;
    procedure CreateRulesForTrue;
    procedure CreateRulesForFalse;
    procedure CreateRulesForNil;
    procedure CreateRulesForBang;
    procedure CreateRulesForPlus;
    procedure CreateRulesForMinus;
    procedure CreateRulesForLessThan;
    procedure CreateRulesForLessThanEqual;
    procedure CreateRulesForGreaterThanEqual;
    procedure CreateRulesForGreaterThan;
    procedure CreateRulesForString;
    procedure CreateRulesForComment;
    procedure CreateRulesForSemiColon;
    procedure CreateRulesForOpenBrace;
    procedure CreateRulesForCloseBrace;
    procedure CreateRulesForPrint;
    procedure CreateRulesForIdentifier;

    procedure CreateRulesForEOF;
  
    procedure CreateRules;
    //--------------------------------------------------------------------------
    Function resolveLocal(Token : pToken) : integer;
    procedure AddLocal(const Token: pToken);
    function  identifiersEqual(const a, b: PToken): Boolean;
    procedure declareVariable();
    procedure Error(Const msg : String);
    function argumentList: Byte;
    procedure NamedVariable(const Token : pToken; const CanAssign : Boolean);
    Procedure variable(const canAssign : boolean);
    function advance : boolean;
    procedure EmitBytes(const Byte1,Byte2 : Byte);
    procedure Number(const canAssign : boolean);
    procedure Binary(const canAssign : boolean);
    procedure Unary(const canAssign : boolean);
    procedure call(const canAssign : boolean);
    procedure literal(const CanAssign : boolean);
    procedure grouping(const canAssign : boolean);
    procedure printStatement;
    procedure ExpressionStatement;
    Procedure beginScope;
    procedure block();
    procedure endScope();
    procedure RemoveLocal;
    Procedure statement;
    procedure declaration;
    procedure varDeclaration;
    function identifierConstant(const token : pToken) : byte;
    function parseVariable(const errorMessage : string) : byte;
    procedure defineVariable(const constantidx : byte);
    procedure parsePrecedence(precedence : TPrecedence);
    function getRule(TokenKind : TTokenKind) : TParseRule;
    procedure consume(const TokenKind : TTokenKind; const Message : String);
  public
    function Chunks : TChunks;
    procedure expression;

    procedure DoCompile;
    constructor Create(Const Scanner : TScanner; Const logging : TStrings);
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
  if FTokens.Current <> nil then
    log('Before Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));
  result := FTokens.MoveNext <> nil;

  if FTokens.Current <> nil then
    log('After Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));

end;



procedure TCompiler.parsePrecedence(precedence : TPrecedence);
var
  prefixRule : TParseFn;
  infixRule : TParseFn;
  canAssign : boolean;
begin
  if not advance then
  begin
    //error('Advance Called when no further tokens' + TTokenName[FTokens.current.kind]);
    exit;
  end;

  prefixRule := getRule(FTokens.Previous.kind).prefix;
  if (@prefixRule = nil) then
  begin
    error('Expected expression. i.e. no prefix rule, when expected one: ' + TTokenName[FTokens.Previous.kind]);
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
    tkBang  : FChunks.ADDNOT;
  end;

end;

procedure TCompiler.expression;
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure TCompiler.consume(const TokenKind : TTokenKind; const Message : String);
begin
  if FTokens.Current.Kind = TokenKind then
  begin
    Advance;
    Exit;
  end;
  error(Message)//errorAtCurrent(message);
end;

procedure TCompiler.Log(const txt : String);
begin
  FLogging.add(txt);
end;

constructor TCompiler.Create(Const Scanner : TScanner; Const logging : TStrings);
begin
  Assert(assigned(Logging), 'No logging injected');
  FLogging := Logging;
  FLogging.clear;
  Assert(Scanner.TokenCount > 1, 'No text to compile');   //it should have at least 1. (regardless of text scanned, as it always adds 1 extra EOF_TOKEN)
  FChunks.Init;
  FScanner := Scanner;

  FTokens.Init(Scanner.Tokens);
  //FTokens.MoveFirst; //is this needed? I think you have to be sitting on 1st token, but at the moment, I can't determine entry point to this compiler in c code-base
  FillChar(FParseRules,sizeof(FParseRules),#0);
  CreateRules;

  FScopeDepth := 0;

  FLocals.Init(true);
end;


procedure TCompiler.CreateRulesForOpenBrace;
begin
  FParseRules[tkOpenBrace].Prefix := nil;
  FParseRules[tkOpenBrace].Infix := nil;
  FParseRules[tkOpenBrace].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForCloseBrace;
begin
  FParseRules[tkCloseBrace].Prefix := nil;
  FParseRules[tkCloseBrace].Infix := nil;
  FParseRules[tkCloseBrace].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForClose_Bracket;
begin

  FParseRules[tkclose_bracket].Prefix := nil;
  FParseRules[tkclose_bracket].Infix := nil;
  FParseRules[tkClose_bracket].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRuleForOpen_bracket;
begin

  FParseRules[tkopen_bracket].Prefix := grouping;
  FParseRules[tkopen_bracket].Infix := call;
  FParseRules[tkopen_bracket].Precedence := PREC_CALL;
end;


procedure TCompiler.CreateRulesForBang;
begin
  // [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE}
  FParseRules[tkBang].Prefix := unary;
  FParseRules[tkBang].Infix := nil;
  FParseRules[tkBang].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForComment;
begin
  FParseRules[tkComment].Prefix := literal;
  FParseRules[tkComment].Infix := nil;
  FParseRules[tkComment].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForDivide;
begin
//  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  FParseRules[tkSlash].Prefix := nil;
  FParseRules[tkSlash].Infix := binary;
  FParseRules[tkSlash].Precedence := PREC_FACTOR;
end;

procedure TCompiler.CreateRulesForNil;
begin
   //[TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  FParseRules[tkNil].Prefix := literal;
  FParseRules[tkNil].Infix := nil;
  FParseRules[tkNil].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForNotEqual;
begin
// [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  FParseRules[tkBangEqual].Prefix := nil;
  FParseRules[tkBangEqual].Infix := binary;
  FParseRules[tkBangEqual].Precedence := PREC_EQUALITY;
end;

procedure TCompiler.CreateRulesForNumber;
begin
  // [TOKEN_NUMBER]   = {number,   NULL,   PREC_NONE},
  FParseRules[tkNumber].Prefix := number;
  FParseRules[tkNumber].Infix := nil;
  FParseRules[tkNumber].Precedence := PREC_NONE;
end;


procedure TCompiler.CreateRulesForEqualEqual;
begin
  //[TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  FParseRules[tkEqualEqual].Prefix := nil;
  FParseRules[tkEqualEqual].Infix := binary;
  FParseRules[tkEqualEqual].Precedence := PREC_EQUALITY;
end;


procedure TCompiler.CreateRulesForFalse;
begin
 //[TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE}
  FParseRules[tkFalse].Prefix := literal;
  FParseRules[tkFalse].Infix := nil;
  FParseRules[tkFalse].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForTrue;
begin
   //[TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE}
  FParseRules[tkTrue].Prefix := literal;
  FParseRules[tkTrue].Infix := nil;
  FParseRules[tkTrue].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForGreaterThan;
begin
  //[TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON}
  FParseRules[tkGreater_Than].Prefix := nil;
  FParseRules[tkGreater_Than].Infix := binary;
  FParseRules[tkGreater_Than].Precedence := PREC_COMPARISON;
end;

procedure TCompiler.CreateRulesForGreaterThanEqual;
begin
  //[TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  FParseRules[tkGreaterThanEqual].Prefix := nil;
  FParseRules[tkGreaterThanEqual].Infix := binary;
  FParseRules[tkGreaterThanEqual].Precedence := PREC_COMPARISON;
end;

procedure TCompiler.CreateRulesForIdentifier;
begin
//   [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE}
  FParseRules[tkIdentifier].Prefix := variable;
  FParseRules[tkIdentifier].Infix := nil;
  FParseRules[tkIdentifier].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForLessThan;
begin
  // [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  FParseRules[tkLess_Than].Prefix := nil;
  FParseRules[tkLess_Than].Infix := binary;
  FParseRules[tkLess_Than].Precedence := PREC_COMPARISON;
end;

procedure TCompiler.CreateRulesForLessThanEqual;
begin
  //[TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  FParseRules[tkLessThanEqual].Prefix := nil;
  FParseRules[tkLessThanEqual].Infix := binary;
  FParseRules[tkLessThanEqual].Precedence := PREC_COMPARISON;
end;

procedure TCompiler.CreateRulesForPlus;
begin
  //[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},

  FParseRules[tkPlus].Prefix := nil;
  FParseRules[tkPlus].Infix := binary;
  FParseRules[tkPlus].Precedence := PREC_TERM;
end;


procedure TCompiler.CreateRulesForPrint;
begin
// [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE}
  FParseRules[tkPrint].Prefix := nil;
  FParseRules[tkPrint].Infix := nil;
  FParseRules[tkPrint].Precedence := PREC_NONE;
end;


procedure TCompiler.CreateRulesForSemiColon;
begin
  //[TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE}
  FParseRules[tkSemiColon].Prefix := nil;
  FParseRules[tkSemiColon].Infix := nil;
  FParseRules[tkSemiColon].Precedence := PREC_NONE;
end;

procedure TCompiler.CreateRulesForString;
begin
   //[TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  FParseRules[tkQuotes].Prefix := strings;
  FParseRules[tkQuotes].Infix := binary;
  FParseRules[tkQuotes].Precedence := PREC_NONE;
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
  CreateRulesForOpenBrace;
  CreateRulesForCloseBrace;
  CreateRulesForNumber;
  CreateRulesForEqualEqual;
  CreateRulesForNotEqual;
  CreateRulesForLessThan;
  CreateRulesForLessThanEqual;
  CreateRulesForGreaterThan;
  CreateRulesForGreaterThanEqual;
  CreateRulesForPlus;
  CreateRulesForMinus;
  CreateRulesForMultiply;
  CreateRulesForDivide;
  CreateRulesForFalse;
  CreateRulesForTrue;
  CreateRulesForNil;
  CreateRulesForBang;
  CreateRulesForComment;
  CreateRulesForString;
  CreateRulesForSemiColon;
  CreateRulesForPrint;
  CreateRulesForIdentifier;
  CreateRulesForEOF;
end;

procedure TCompiler.printStatement;
begin
  expression;
  consume(tkSemiColon, 'Expect ";" after value.');
  FChunks.ADDPRINT;
end;

procedure TCompiler.ExpressionStatement;
begin
  Expression;
  Consume(tkSemiColon, 'Expect ";" after value.');
  FChunks.ADDPOP;
end;


(*
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
*)


Function TCompiler.resolveLocal(Token : pToken) : integer;
var
  i : integer;
  local : pLocal;

  a,b : string;

begin
  result := -1;

  a := FScanner.ln.items[Token.Line].text;
  a := copy(a,token.Start,token.length);

  for i := FLocals.Count-1 downto 0 do
  begin
    Local := Flocals[i];

    b := FScanner.ln.items[Local.Token.Line].text;
    b := copy(b,local.token.Start,local.token.length);

    if identifiersEqual(Token, local.Token) then
    begin
      if (local.depth = -1) then
      begin
        error('Can''t read local variable in its own initializer.');
      end;


      result :=  i;
      Log(format('resolved local for %s. found at local index index %d',[TokenName(Token),i]));
      exit;
    end;
  end;
  Log(format('unable to resolve local for %s.',[TokenName(Token)]));
end;

procedure TCompiler.NamedVariable(const Token : pToken;const CanAssign : Boolean);
var
  getOp,setOp : TOpCodes;
  Idx : integer;
  arg : integer;
begin

  Idx := resolveLocal(Token);
  //assert(global <> -1, 'Could not resolve local for token');
  if (Idx <> -1) then
  begin
    getOp := OP_GET_LOCAL;
    setOp := OP_SET_LOCAL;
  end
  else
  begin
    Log(format('local for %s not found in scope. Must be global',[TokenName(Token)]));

    Idx := identifierConstant(Token);
    getOp := OP_GET_GLOBAL;
    setOp := OP_SET_GLOBAL;
  end;

  if canAssign and (match(tkEqual)) then
  begin
     Log(format('Entering expression evaluation for assignable var',[TokenName(Token)]));

     expression();
     Log(format('Exited expression evaluation for assignable var',[TokenName(Token)]));

     //FChunks.AddSET_GLOBAL(global);
     FChunks.emitBytes(byte(setOp), Idx);

     Log(format('Adding byte for opcode %s, pointing to idx : %d',[opCodeToStr(setOp), idx]));
   end
   else
   begin
     //FChunks.AddGET_GLOBAL(global);
     Log('non assignable token');
     FChunks.emitBytes(byte(getOp), Idx);
     Log(format('Adding byte for opcode %s, pointing to index : %d ',[opCodeToStr(getOp), idx]));
   end;
end;

Procedure TCompiler.variable(const CanAssign : boolean);
begin
  namedVariable(FTokens.previous, CanAssign);
end;


procedure TCompiler.defineVariable(const constantIdx : byte);
begin
  log('define variable');
  if (FscopeDepth > 0) then
  begin
    log('scope depth is > 0 therefore not global');
    exit;
  end;

  log('define global variable');
  FChunks.AddDEFINE_GLOBAL(constantidx);
end; {
  emitBytes(OP_DEFINE_GLOBAL, global);
}


function TCompiler.identifierConstant(const token : pToken) : byte;
var
  value : pValue;
  text : string;
begin
  assert(token <> nil,'Name is nil, cannot add identified constant');
  text := FScanner.ln.items[Token.Line].text;
  text := copy(text,token.Start,token.length);
  Value :=  NewString(Text);//StringValue(text);
  result := FChunks.MakeConstant(Value);
end;


function TCompiler.parseVariable(const errorMessage : string) : byte;
begin
  Log('Parse variable');
  consume(tkIdentifier, errorMessage);
  DeclareVariable;
  if FScopeDepth > 0 then
  begin
    result := 0;
    exit;
  end;
  result := identifierConstant(FTokens.previous); //index of the variable name in the constant table
end;


function TCompiler.TokenName(const token : pToken) : String;
begin
  result := FScanner.ln.items[token.Line].text;
  result := copy(result,token.Start,token.length);
end;

function TCompiler.identifiersEqual(const a, b: PToken): Boolean;
var
  txt1,txt2 : string;
begin
  false;
  if a.kind <> b.kind then exit;

  txt1 := FScanner.ln.items[a.Line].text;
  txt1 := copy(txt1,a.Start,a.length);

  txt2 := FScanner.ln.items[b.Line].text;
  txt2 := copy(txt2,b.Start,b.length);


  result := txt1 = txt2;
end;

procedure TCompiler.AddLocal(const Token: pToken);
var
  Local: PLocal;
begin
  if FLocals.Count = MAX_LOCALS then
  begin
    Error('Too many local variables in function.');
    Exit;
  end;
  new(Local);
  Local.Token := Token;
  local.depth := FScopeDepth;
  Local.isCaptured := False; //presumably for future captures
  FLocals.Add(Local);
  Log(format('Adding new local %s, %s. Current local count : %d',[TokenKindToStr(Token.Kind),TokenName(Token),FLocals.Count]));
end;


procedure TCompiler.declareVariable();
var
  i: Integer;
  token: pToken; // Assuming PToken is a pointer to the Token struct.
  local : pLocal;
begin
  if FScopeDepth = 0 then
    Exit;

  token := FTokens.previous;

  for i := Flocals.Count - 1 downto 0 do
  begin
    local := Flocals[i];
    if (local.depth <> -1) and (local.depth < FScopeDepth) then
    begin
      Break;
    end;

    if identifiersEqual(token, Flocals[i].token) then
    begin
      error('Already a variable with this name in this scope.');
      exit;
    end;
  end;

  addLocal(token);
end;


procedure TCompiler.varDeclaration;
var
  constantIdx : Byte;
begin

  constantIdx := parseVariable('Expect variable name.');

  if (match(tkequal)) then
  begin
    expression();
  end
  else
  begin
    FChunks.ADDNil;
  end;
  consume(tkSemiColon,'Expect ";" after variable declaration.');

  defineVariable(constantidx);
end;

Procedure TCompiler.beginScope;
begin
  inc(FScopeDepth);
  Log(format('Begin Scope : %d',[FScopeDepth]));
end;


procedure TCompiler.block();
begin
  Log(format('Begin block at : %d',[FScopeDepth]));
  while (not checkKind(tkCloseBrace) and not checkKind(tkEOF)) do
  begin
    declaration();
  end;
  consume(tkCloseBrace, 'Expect "}" after block.');
  Log(format('end block at : %d',[FScopeDepth]));
end;


procedure TCompiler.endScope();
var
  Local : pLocal;
  i : integer;
  scope : string;
begin
  Log(format('End scope : %d',[FScopeDepth]));
  dec(FScopeDepth);
  removeLocal;
end;

procedure TCompiler.RemoveLocal;
var
  Local : pLocal;
begin

  while (Flocals.Count > 0) and (FLocals.Last.depth > FscopeDepth) do
  begin
    FChunks.AddPOP;
    Local := FLocals.Remove(FLocals.Count-1);
    Dispose(Local);
    Log(format('Remove local kind : %s, name : %s, local count : %d',[TokenKindToStr(Local.Token.Kind),TokenName(Local.Token),FLocals.Count]));
  end;
end;

//[1,1,2,2,1,1]



procedure TCompiler.statement;
begin
  if (match(tkPRINT)) then
  begin
    printStatement
  end
  else
  if (match(tkOpenBrace)) then
  begin
    beginScope;
    block;
    endScope;
  end
  else
  if (match(tkVar)) then
  begin
    varDeclaration;
  end
  else
  begin
    expressionStatement;
//    FTokens.MoveLast;
  end;
End;

procedure TCompiler.Error(const msg: String);
begin
  raise exception.create(msg);
end;

procedure TCompiler.Number(const canAssign : boolean);
var
  token : pToken;
  number : double;
  text : string;
  Value : pValue;
begin
  Token := FTokens.Previous;
  if Token = nil then exit;

  text := FScanner.ln.items[Token.Line].text;
  text := copy(text,token.Start,token.length);

  Value := NewNumber(strToFloat(text));

  FChunks.EmitConstant(Value);
end;

procedure TCompiler.grouping(const canAssign : boolean);
begin
   expression();
   consume(tkclose_bracket, 'Expect '')'' after expression.');  //right bracket
end;

procedure TCompiler.literal(const CanAssign: boolean);
begin
  case  FTokens.Previous.Kind of
    tkFalse : FChunks.AddFALSE;
    tknil   : FChunks.AddNil;
    tkTrue  : FChunks.AddTrue;
  end;
end;


function TCompiler.checkKind(Kind : TTokenKind) : boolean;
begin
  result := FTokens.Current.Kind = Kind;
end;

function TCompiler.match(const Expected : TTokenKind) : boolean;
begin
  result := CheckKind(Expected);
  if not result then exit;
  advance;
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


procedure TCompiler.Strings(const canAssign: Boolean);
var
  Value : pValue;
  Token : pToken;
  Text  : String;
begin
   Token := FTokens.previous;
   Text :=  FScanner.ln.items[Token.Line].text;
   text := copy(text,token.Start+1,token.length-2);
   Value := NewString(Text);
   FChunks.EmitConstant(Value);
  // FChunks.AddConstant(StringValue(Copy(
  // emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
  //                                parser.previous.length - 2))); *)
end;

procedure TCompiler.EmitBytes(const Byte1,Byte2 : Byte);
begin

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

procedure TCompiler.DoCompile;
begin
  advance;
  while not Match(tkEOF) do
  begin
    Declaration;
  end;
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
    TkPlus              : FChunks.AddADD;
    TkMinus             : FChunks.AddSUBTRACT;
    tkAsterisk          : FChunks.AddMULTIPLY;
    tkEqualEqual        : FChunks.AddEQUAL;
    tkBangEqual         : FChunks.AddNOTEqual;
    tkLess_Than         : FChunks.AddLess;
    tkLessThanEqual     : FChunks.AddLESSTHANEQUAL;
    tkGreaterThanEqual  : FChunks.AddGREATERTHANEQUAL;
    tkgreater_than      : FChunks.AddGREATER;
    tkSlash             : FChunks.AddDivide;
    else
    begin
      raise exception.create('A token kind not catered for in a binary operation was encountered');
    end;
  end

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


procedure TCompiler.declaration;
begin
   if match(tkVar) then
     varDeclaration()
   else
     statement();
end;

destructor TCompiler.destroy;
begin
  FChunks.Finalize;
  FLocals.Finalize;
  inherited;
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






