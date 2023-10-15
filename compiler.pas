unit compiler;

interface

uses classes,LOXTypes, TokenArray, Scanner, Chunk, locals, objectFunction,
    ScopeDepth;

const
  Start_Scope_depth = 10;

type

 TParseFn = procedure(const canAssign : boolean) of object;

 


  pParseRule = ^TParseRule;
  TParseRule = record
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkEOF] of pParseRule;

  TCompiler = class
  private
    FScopeDepth : TScopeDepth;

    FFunc : pLoxFunction;
    FFunctionKind : TFunctionKind;

    FParseRules : TParseRules;




    Flocals : TLocalList;
    FChunks : TChunks;

    FTokens : TTokenIterator;
    FScanner : TScanner;
//    procedure emitConstant(value : TValue);
 //   function NumberVal(num: Integer): TValue;
 //   function NumToValue(num: Double): TValue;


    //--------------------------------------------------------------------------
    procedure patchJump(const offset : integer);
    function emitJump(const instruction : TOpCodes) : integer;
    procedure ifStatement;
    function TokenName(const token : pToken) : String;


    function  checkKind(Kind : TTokenKind) : boolean;
    function  match(const Expected : TTokenKind) : boolean;
    procedure whileStatement();
    procedure emitLoop(const loopStart : integer);

    Function resolveLocal(Token : pToken) : integer;
    procedure AddLocal(
      const Token: pToken;
      const ScopeDepth : integer);
    function  identifiersEqual(const a, b: PToken): Boolean;
    procedure declareVariable();
    procedure Error(Const msg : String);
    function argumentList: Byte;
    procedure NamedVariable(const Token : pToken; const CanAssign : Boolean);
    function advance : boolean;





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
    procedure defineVariable(const constantidx : integer);

    procedure parsePrecedence(precedence : TPrecedence);



    procedure consume(const TokenKind : TTokenKind; const Message : String);
    function GetParseRule(const TokenKind: TTokenKind): pParseRule;
    procedure SetParseRule(const TokenKind: TTokenKind;
      const Value: pParseRule);
  public
    Procedure and_(const canAssign : boolean);
    Procedure or_(const canAssign : boolean);
    Procedure variable(const canAssign : boolean);
    procedure Strings(const canAssign: Boolean);
    procedure EmitBytes(const Byte1,Byte2 : Byte);
    procedure Number(const canAssign : boolean);
    procedure Binary(const canAssign : boolean);
    procedure Unary(const canAssign : boolean);
    procedure call(const canAssign : boolean);
    procedure literal(const CanAssign : boolean);
    procedure grouping(const canAssign : boolean);

    function Chunks : TChunks;
    procedure expression;

    procedure DoCompile;
    constructor Create(

        Const Scanner : TScanner;
        Const FunctionKind : TFunctionKind);
    destructor destroy;override;


    property ParseRule[const TokenKind : TTokenKind] : pParseRule
      read   GetParseRule
      write  SetParseRule;
  end;



implementation

uses sysutils;


function TCompiler.advance : boolean;
begin

  result := FTokens.MoveNext <> nil;


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
   consume(tkclosebracket, 'Expect '')'' after expression.');  //right bracket
end;

procedure TCompiler.literal(const CanAssign: boolean);
begin
  case  FTokens.Previous.Kind of
    tkFalse : FChunks.AddFALSE;
    tknil   : FChunks.AddNil;
    tkTrue  : FChunks.AddTrue;
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


constructor TCompiler.Create(
  Const Scanner : TScanner;
  Const FunctionKind : TFunctionKind);
begin

  Assert(Scanner.TokenCount > 1, 'No text to compile');   //it should have at least 1. (regardless of text scanned, as it always adds 1 extra EOF_TOKEN)
  FChunks.Init;
  FScanner := Scanner;

  FTokens.Init(Scanner.Tokens);
  //FTokens.MoveFirst; //is this needed? I think you have to be sitting on 1st token, but at the moment, I can't determine entry point to this compiler in c code-base
  FillChar(FParseRules,sizeof(FParseRules),#0);


  FScopeDepth := TScopeDepth.Create(Start_Scope_depth);


  //FScopeDepth := Start_Scope_depth;

  FLocals.Init(true);
  //AddLocal(nil,0); //add an empty local for later use internally.

  (*

  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->name.start = "";
  local->name.length = 0;

  *)


  FFunctionKind := FunctionKind;
  FFunc := newLoxFunction('TopLevelCompiler');
  
end;


//note book suggests better ways to do this, but this uses existing functionality.
Procedure TCompiler.or_(const canAssign : boolean);
var
  elseJump,endJump : integer;
begin
  elseJump := emitJump(OP_JUMP_IF_FALSE);
  endJump := emitJump(OP_JUMP);

  patchJump(elseJump);
  FChunks.ADDPOP;

  parsePrecedence(PREC_OR);
  patchJump(endJump);
end;



Procedure TCompiler.and_(const canAssign : boolean);
var
  endJump : integer;
begin
  endJump := emitJump(OP_JUMP_IF_FALSE);
  FChunks.AddPOP;
  parsePrecedence(PREC_AND);
  patchJump(endJump);
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


function TCompiler.GetParseRule(const TokenKind: TTokenKind): pParseRule;
begin
   result := FParseRules[TokenKind];
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

  for i := FLocals.Count-1 downto FScopeDepth.StartScopeDepth do
  begin
    Local := Flocals[i];
    if not assigned(Local.Token) then continue; //1st item now has nil token and used internally
    b := FScanner.ln.items[Local.Token.Line].text;
    b := copy(b,local.token.Start,local.token.length);

    if identifiersEqual(Token, local.Token) then
    begin
      if (local.depth = -1) then
      begin
        error('Can''t read local variable in its own initializer.');
      end;


      result :=  i;

      exit;
    end;
  end;

end;

procedure TCompiler.SetParseRule(const TokenKind: TTokenKind;
  const Value: pParseRule);
begin
  FParseRules[TokenKind] := Value;
end;

procedure TCompiler.NamedVariable(const Token : pToken;const CanAssign : Boolean);
var
  getOp,setOp : TOpCodes;
  Idx : integer;
  arg : integer;
begin

  Idx := resolveLocal(Token);

  if (Idx <> -1) then
  begin
    getOp := OP_GET_LOCAL;
    setOp := OP_SET_LOCAL;
  end
  else
  begin


    Idx := identifierConstant(Token);
    getOp := OP_GET_GLOBAL;
    setOp := OP_SET_GLOBAL;
  end;

  if canAssign and (match(tkEqual)) then
  begin
      

     expression();


     //FChunks.AddSET_GLOBAL(global);
     FChunks.emitBytes(byte(setOp), Idx);


   end
   else
   begin
     //FChunks.AddGET_GLOBAL(global);

     FChunks.emitBytes(byte(getOp), Idx);

   end;
end;

Procedure TCompiler.variable(const CanAssign : boolean);
begin
  namedVariable(FTokens.previous, CanAssign);
end;

//define globals? 
procedure TCompiler.defineVariable(const constantIdx : integer);
begin
  if (FscopeDepth.ScopeDepth > Start_Scope_depth) then
  begin
    exit;
  end;
  FChunks.AddDEFINE_GLOBAL(constantidx);
end; {
  emitBytes(OP_DEFINE_GLOBAL, global);
}


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

  prefixRule := FParseRules[FTokens.Previous.kind].prefix;
  if (@prefixRule = nil) then
  begin
    error('Expected expression. i.e. no prefix rule, when expected one: ' + TTokenName[FTokens.Previous.kind]);
    exit;
  end;

  canAssign := precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= FParseRules[FTokens.current.Kind].precedence) do
  begin
    if not advance then exit;
    infixRule := FParseRules[FTokens.previous.Kind].infix;
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

  consume(tkIdentifier, errorMessage);
  DeclareVariable;
  if FScopeDepth.ScopeDepth > Start_Scope_depth then
  begin
    result := Start_Scope_depth;
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

procedure TCompiler.AddLocal(
  const Token: pToken;
  const ScopeDepth : integer);
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
  local.depth := ScopeDepth;
  Local.isCaptured := False; //presumably for future captures
  FLocals.Add(Local);

end;


procedure TCompiler.declareVariable();
var
  i: Integer;
  token: pToken; // Assuming PToken is a pointer to the Token struct.
  local : pLocal;
begin
  if FScopeDepth.ScopeDepth = Start_Scope_depth then
    exit;

  token := FTokens.previous;

  //look for duplicate vars in same scope
  for i := Flocals.Count - 1 downto Start_Scope_depth do
  begin
    local := Flocals[i];
    if (local.depth <> -1) and (local.depth < FScopeDepth.ScopeDepth) then
    begin
      Break;
    end;

    if identifiersEqual(token, Flocals[i].token) then
    begin
      error('Already a variable with this name in this scope.');
      exit;
    end;
  end;

  addLocal(token,FScopeDepth.ScopeDepth);
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
  FScopeDepth.push;
end;


procedure TCompiler.block();
begin

  while (not checkKind(tkCloseBrace) and not checkKind(tkEOF)) do
  begin
    declaration();
  end;
  consume(tkCloseBrace, 'Expect "}" after block.');
end;


procedure TCompiler.endScope();
var
  Local : pLocal;
  i : integer;
  scope : string;
begin
  FScopeDepth.Pop;
  removeLocal;
end;

procedure TCompiler.RemoveLocal;
var
  Local : pLocal;
begin

  while (Flocals.Count > 0)   and
        (FLocals.Last <> nil) and
        (FLocals.Last.depth > FscopeDepth.ScopeDepth) do
  begin
    FChunks.AddPOP;
    Local := FLocals.Remove(FLocals.Count-1);
    Dispose(Local);
  end;
end;

procedure TCompiler.PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  // -2 to adjust for the bytecode for the jump offset itself.
  Jump := FChunks.OPCodes.Count - OffSet - 2;

  if Jump > MAX_JUMP then
    Error('Too much code to jump over.');

  FChunks.OpCodes.Item[OffSet]^   := (Jump shr 8) and $FF;
  FChunks.OpCodes.Item[OffSet+1]^ := Jump and $FF;
end;

function TCompiler.emitJump(const instruction : TOpCodes) : integer;
begin
  FChunks.emitByte(Byte(instruction));
  FChunks.emitByte($FF);
  FChunks.emitByte($FF);
  result :=  FChunks.OpCodes.count - 2;
end;

procedure TCompiler.ifStatement;
var
  thenJump,elseJump : integer;
begin
  consume(tkOpenBracket,'Expect "(" after "if".');
  expression();
  consume(tkCloseBracket, 'Expect ")" after condition.');

  thenJump := emitJump(OP_JUMP_IF_FALSE);

  statement();

  elseJump := emitJump(OP_JUMP);
  patchJump(thenJump);

  if (match(tkElse)) then statement();

  patchJump(elseJump);
end;



procedure TCompiler.emitLoop(const loopStart : integer);
var
  offset : integer;
begin
  FChunks.emitByte(OP_LOOP);

  offset := FChunks.OpCodes.count - loopStart + 2;
  if (offset > MAX_JUMP) then error('Loop body too large.');

  FChunks.emitByte((offset shr 8) and $ff);
  FChunks.emitByte(offset and $ff);

end;

procedure TCompiler.whileStatement();
var
  exitJump : integer;
  loopStart : integer;
begin
  
  loopStart := FChunks.OpCodes.count;
  consume(tkOpenBracket, 'Expect "(" after while.');
  expression();
  consume(tkCloseBracket, 'Expect ")" after condition.');
  exitJump := emitJump(OP_JUMP_IF_FALSE);
  FChunks.addPOP;
  statement();
  emitLoop(loopStart);
  patchJump(exitJump);
  FChunks.addPOP;
end;
 
procedure TCompiler.statement;
begin
  if (match(tkComment)) then
  begin
    exit;
  end;

  if (match(tkPRINT)) then
  begin
    printStatement;
    exit;
  end;

  if (match(tkIF))then
  begin
    ifStatement();
    exit;
  end;

  if (match(tkOpenBrace)) then
  begin
    beginScope;
    block;
    endScope;
    exit;
  end;

  if (match(tkVar)) then
  begin
    varDeclaration;
    exit;
  end;

  if (match(tkWhile)) then
  begin
    whileStatement();
    exit;
  end;


  expressionStatement;


end;

procedure TCompiler.Error(const msg: String);
begin
  raise exception.create(msg);
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
  if not (FTokens.Current.Kind = tkcloseBracket) then  //check(TOKEN_RIGHT_PAREN) then
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
  consume(tkcloseBracket, 'Expect '')'' after arguments.');
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

//virtual machine calls compile from interpret...
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
  rule : pParseRule;
begin
  TokenKind := FTokens.Previous.Kind;
  rule := FParseRules[FTokens.Previous.Kind];
  parsePrecedence(TPrecedence(ord(rule.Precedence) + 1));


  Case (TokenKind) of
    TkPlus              : FChunks.AddADD;
    TkMinus             : FChunks.AddSUBTRACT;
    tkAsterisk          : FChunks.AddMULTIPLY;
    tkEqualEqual        : FChunks.AddEQUAL;
    tkBangEqual         : FChunks.AddNOTEqual;
    tkLessThan          : FChunks.AddLess;
    tkLessThanEqual     : FChunks.AddLESSTHANEQUAL;
    tkGreaterThanEqual  : FChunks.AddGREATERTHANEQUAL;
    tkgreaterthan       : FChunks.AddGREATER;
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
  FScopeDepth.Free;
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






