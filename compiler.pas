unit compiler;

interface

uses dialogs,classes,LOXTypes, TokenArray, Scanner, locals,values, opCodes;


type

 TParseFn = procedure(const canAssign : boolean) of object;
 
  pParseRule = ^TParseRule;
  TParseRule = record
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkEOF] of pParseRule;


  //TCompilerController = class;

  TCompiler = class
  private
    FInternal       : TToken;
    FName           : String;
    FEnclosing      : TCompiler;
    Flocals         : TLocals;
    FFunc           : pLoxFunction;
    FFunctionKind   : TFunctionKind;
    FScopeDepth     : integer;
    FLocalCount     : integer;
  protected
    function GetLocals : TLocals;
  public
    procedure ToString(const strings : TStrings);

    constructor create(
      const Name : string;
      const FunctionKind : TFunctionKind;
      const Enclosing    : TCompiler);
    destructor destroy;  override;

    property Func       : pLoxFunction read FFunc write FFunc;
    property FuncKind   : TFunctionKind read FFunctionKind write FFunctionKind;
    property ScopeDepth : integer read fScopeDepth write fScopeDepth;
    property Locals     : TLocals read GetLocals;
    property Enclosing  : TCompiler read FEnclosing write FEnclosing;
    property Name : String read FName;
  end;

  TCompilers = class
  private
    FItems : TList;
    function GetCurrent: TCompiler;
    function getCOunt: integer;
  protected
    function getCompiler(const Index: integer): TCompiler;
    procedure setCompiler(const Index: integer; const Value: TCompiler);
  public
    function Add(
      const Name : string;
      const FunctionKind : TFunctionKind;
      const Enclosing    : TCompiler): TCompiler;
    constructor create;
    destructor destroy;override;
    property Compiler[Const Index : integer] : TCompiler read getCompiler write setCompiler;default;
    property Current : TCompiler read GetCurrent;
    property Count : integer read getCOunt;
  end;

  TCompilerController = class
  private

    FCompilers : TCompilers;

    //FGlobals : TValueList;   //scope wide

    FCurrent : TCompiler;

    FParseRules : TParseRules;

    FTokens : TTokenIterator;
    FScanner : TScanner;

    Function TokenName(const Token : TToken) : String;


    //---------------------Compiler rules --------------------------------------
    procedure SetRulesForOpenSquareBracket;
    procedure SetRulesForCloseSquareBracket;


    procedure  SetRulesForOpenBrace;
    procedure  SetRulesForCloseBrace;
    procedure  SetRulesForClose_Bracket;
    procedure  SetRuleForOpen_bracket;
    procedure  SetRulesForBang;
    procedure  SetRulesForComment;
    procedure  SetRulesForDivide;
    procedure  SetRulesForNil;
    procedure  SetRulesForNotEqual;
    procedure  SetRulesForNumber;
    procedure  SetRulesForEqualEqual;
    procedure  SetRulesForFalse;
    procedure  SetRulesForTrue;
    procedure  SetRulesForWhile;
    procedure  SetRulesForGreaterThan;
    procedure  SetRulesForGreaterThanEqual;
    procedure  SetRulesForIdentifier;
    procedure  SetRulesForIF;
    procedure  SetRulesForLessThan;
    procedure  SetRulesForLessThanEqual;
    procedure  SetRulesForPlus;
    procedure  SetRulesForPrint;
    procedure  SetRulesForSemiColon;
    procedure  SetRulesForString;
    procedure  SetRulesForMinus;
    procedure  SetRulesForMultiply;
    procedure  SetRulesForElse;
    procedure  SetRulesForEOF;
    procedure  SetRulesForOR;
    procedure  SetRulesForAnd;
    procedure  SetRulesForComma;
    procedure  SetRulesForFun;
    procedure  SetRules;


    //--------------------------------------------------------------------------
    procedure EmitReturn;
    function EndCompiler: pLoxFunction;


    procedure markInitialized();
    procedure funDeclaration;
    procedure patchJump(const offset : integer);
    function emitJump(const instruction : TOpCodes) : integer;
    procedure ifStatement;
    procedure Log(const txt : String);

    function  checkKind(const Kind : TTokenKind) : boolean;
    function  match(const Expected : TTokenKind) : boolean;
    procedure whileStatement();
    procedure returnStatement();
    procedure emitLoop(const loopStart : integer);

    Function resolveLocal(
      const Compiler : TCompiler;
      const Token : TToken) : integer;
    procedure AddLocal(const Token: TToken);
    function  identifiersEqual(const a, b: TToken): Boolean;
    procedure declareLocalVariable();
    procedure Error(Const msg : String);
    function argumentList: Byte;
    procedure NamedVariable(const Token : TToken; const CanAssign : Boolean);
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

    procedure ListInit(const canAssign: Boolean);
    procedure Subscript(const canAssign: Boolean);
    Procedure and_(const canAssign : boolean);
    Procedure or_(const canAssign : boolean);
    Procedure variable(const canAssign : boolean);
    procedure Strings(const canAssign: Boolean);
    procedure Number(const canAssign : boolean);
    procedure Binary(const canAssign : boolean);
    procedure Unary(const canAssign : boolean);
    procedure call(const canAssign : boolean);
    procedure literal(const CanAssign : boolean);
    procedure grouping(const canAssign : boolean);

//    function Chunks : TChunks;
    procedure expression;

    function parseVariable(const errorMessage : string) : integer;
    procedure defineVariable(const constantidx : integer);

    procedure parsePrecedence(precedence : TPrecedence);

    procedure DoFunction (FunctionKind : TFunctionKind);

    procedure consume(const TokenKind : TTokenKind; const Message : String);
    function GetParseRule(const TokenKind: TTokenKind): pParseRule;
    procedure SetParseRule(const TokenKind: TTokenKind;
      const Value: pParseRule);
    function getCount: integer;
    function getCompiler(const index: integer): TCompiler;
  public
    procedure LocalsToString(const strings : TStrings);


    function DoCompile : pLoxFunction;

   constructor Create(
       const Tokens  : TTokenIterator;
       const Scanner : TScanner;
       const FunctionKind : TFunctionKind);

    destructor destroy;override;

    property Current : TCompiler read fCurrent write fCurrent;
    property ParseRule[const TokenKind : TTokenKind] : pParseRule
      read   GetParseRule
      write  SetParseRule;
    property Count : integer read getCount;
    property Compiler[const index : integer] : TCompiler read getCompiler; default;
  end;



implementation

uses sysutils, valueManager;


function TCompilerController.advance : boolean;
begin
  //if FTokens.Current <> nil then
  //  log('Before Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));
  result := FTokens.MoveNext <> nil;

  //if FTokens.Current <> nil then
  //  log('After Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));

end;

procedure TCompilerController.Number(const canAssign : boolean);
var
  token : TToken;
  number : double;
  text : string;
  Value : pValueRecord;

begin
  Token := FTokens.Previous;
  if Token = nil then exit;

  text := TokenName(Token);

  Value := BorrowChecker.NewNumber(strToFloat(text));

  Current.Func.Chunks.EmitConstant(Value);
end;



procedure TCompilerController.grouping(const canAssign : boolean);
begin
   expression();
   consume(tkclosebracket, 'Expect '')'' after expression.');  //right bracket
end;

procedure TCompilerController.literal(const CanAssign: boolean);
begin
  case  FTokens.Previous.Kind of
    tkFalse : Current.Func.Chunks.AddFALSE;
    tknil   : Current.Func.Chunks.AddNil;
    tkTrue  : Current.Func.Chunks.AddTrue;
  end;
end;

procedure TCompilerController.Unary(const canAssign: boolean);
var
  TokenKind : TTokenKind;
begin
  TokenKind := FTokens.previous.Kind;
  parsePrecedence(PREC_UNARY);
  case TokenKind of
    tkMinus : Current.Func.Chunks.AddNEGATE;
    tkBang  : Current.Func.Chunks.ADDNOT;
  end;

end;

procedure TCompilerController.expression;
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure TCompilerController.consume(const TokenKind : TTokenKind; const Message : String);
begin
  if FTokens.Current.Kind = TokenKind then
  begin
    Advance;
    Exit;
  end;
  error(Message)//errorAtCurrent(message);
end;

procedure TCompilerController.Log(const txt : String);
begin

end;


//note book suggests better ways to do this, but this uses existing functionality.
Procedure TCompilerController.or_(const canAssign : boolean);
var
  elseJump,endJump : integer;
begin
  elseJump := emitJump(OP_JUMP_IF_FALSE);
  endJump := emitJump(OP_JUMP);

  patchJump(elseJump);
  Current.Func.Chunks.ADDPOP;

  parsePrecedence(PREC_OR);
  patchJump(endJump);
end;



Procedure TCompilerController.and_(const canAssign : boolean);
var
  endJump : integer;
begin
  endJump := emitJump(OP_JUMP_IF_FALSE);
  Current.Func.Chunks.AddPOP;
  parsePrecedence(PREC_AND);
  patchJump(endJump);
end;


procedure TCompilerController.printStatement;
begin
  expression;
  consume(tkSemiColon, 'Expect ";" after value.');
  Current.Func.Chunks.ADDPRINT;
end;

procedure TCompilerController.ExpressionStatement;
begin
  Expression;
  Consume(tkSemiColon, 'Expect ";" after value.');
  Current.Func.Chunks.ADDPOP;
end;


function TCompilerController.GetParseRule(const TokenKind: TTokenKind): pParseRule;
begin
   result := FParseRules[TokenKind];
end;
 


Function TCompilerController.TokenName(const Token : TToken) : String;
var
  a : string;
begin
  a := FScanner.ln.items[Token.Line].text;
  a := copy(a,token.Start,token.length);
  result := a;
end;

Function TCompilerController.resolveLocal(
  const Compiler : TCompiler;
  const Token : TToken) : integer;
var
  i : integer;
  local : TLocal;

  a,b : string;

begin
  result := -1;

  a := TokenName(Token);

  for i := Compiler.Locals.Count-1 downto 0 do
  begin
    Local := Compiler.locals[i];
    if not assigned(Local.Token) then continue; //1st item now has nil token and used internally

    b := TokenName(Local.Token);

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


//{ list, subscript, PREC_SUBSCRIPT }, // TOKEN_LEFT_BRACKET
procedure TCompilerController.SetRulesForOpenSquareBracket;
var
  parseRule : pParseRule;

begin
  new(parseRule);

  ParseRule.Prefix := ListInit;
  ParseRule.Infix :=  Subscript;
  ParseRule.Precedence := PREC_SUBScript;
  FParseRules[tkOpenSquareBracket] := ParseRule;
end;

procedure TCompilerController.SetRulesForCloseSquareBracket;
var
  parseRule : pParseRule;
begin

  new(parseRule);

  ParseRule .Prefix := nil;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkCloseSquareBracket] := ParseRule;
end;

procedure TCompilerController.SetParseRule(const TokenKind: TTokenKind;
  const Value: pParseRule);
begin
  FParseRules[TokenKind] := Value;
end;


procedure TCompilerController.NamedVariable(const Token : TToken;const CanAssign : Boolean);
var
   getOp,setOp : TOpCodes;
   Idx : integer;
   arg : integer;
   Value : pValueRecord;
begin

  Idx := resolveLocal(Current,Token);

  if (Idx <> -1) then
  begin
    getOp := OP_GET_LOCAL;
    setOp := OP_SET_LOCAL;
  end
  else
  begin
    Value := BorrowChecker.NewString(rCompiler,TokenName(token));
    idx :=   Current.Func.Chunks.AddConstant(Value);
    getOp := OP_GET_GLOBAL;
    setOp := OP_SET_GLOBAL;
  end;



  if canAssign and (match(tkEqual)) then
  begin
     expression();

     Current.Func.Chunks.Emit(setOp, Idx);

   end
   else
   begin
     Current.Func.Chunks.Emit(getOp, Idx);
   end;
end;

Procedure TCompilerController.variable(const CanAssign : boolean);
begin
  namedVariable(FTokens.previous, CanAssign);
end;


procedure TCompilerController.markInitialized();
begin
  if Current.ScopeDepth = 0 then exit;
  Current.Locals.Last.Depth := Current.ScopeDepth;
end;


procedure TCompilerController.defineVariable(const constantIdx : integer);
begin
  log('define variable');
  if (Current.scopeDepth > 0) then  // If we're in a nested scope, mark the variable as initialized locally.
  begin
    log('scope depth is > 0 therefore not global');
    MarkInitialized;
    exit;
  end;

  // If we're in the outermost scope, define the variable as a global.
  log('define global variable');
  Current.Func.Chunks.AddDEFINE_GLOBAL(constantidx);
end;

procedure TCompilerController.parsePrecedence(precedence : TPrecedence);
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


(*
  static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
//> Local Variables parse-local

  declareVariable();
  if (current->scopeDepth > 0) return 0;

//< Local Variables parse-local
  return identifierConstant(&parser.previous);
}

*)


function TCompilerController.parseVariable(const errorMessage : string) : integer;
var
  Value : pValueRecord;
  Name  : String;
begin
  consume(tkIdentifier, errorMessage);

  if Current.ScopeDepth > 0 then
  begin
    declareLocalVariable;
    result := 0;
    exit;
  end;

  Value  := BorrowChecker.NewString(rCompiler,TokenName(FTokens.previous));
  result := Current.Func.Chunks.AddConstant(Value);
end;


function TCompilerController.identifiersEqual(const a, b: TToken): Boolean;
var
  txt1,txt2 : string;
begin
  false;

  if a.kind <> b.kind then exit;

  txt1 := TokenName(a);

  txt2 := TokenName(b);

  result := txt1 = txt2;
end;


procedure TCompilerController.RemoveLocal;
var
  Local : TLocal;
begin

  while (Current.locals.Count > 0) and (Current.Locals.Last.depth > Current.scopeDepth) do
  begin
    Current.Func.Chunks.AddPOP;
    Current.Locals.Remove(Current.Locals.Count-1);

    //Log(format('Remove local kind : %s, name : %s, local count : %d',[TokenKindToStr(Local.Token.Kind),TokenName(Local.Token),FLocals.Count]));
  end;
end;

procedure TCompilerController.AddLocal(const Token: TToken);
var
  Local: TLocal;
begin
  Assert(Assigned(token),'token being added to local is nil');
  if Current.Locals.Count = MAX_LOCALS then
  begin
    Error('Too many local variables in function.');
    Exit;
  end;
  Local := Current.Locals.Add(TokenName(Token), token);
//  Local.Token := Token;
  local.depth := -1 ; //declare undefined
  Local.isCaptured := False; //presumably for future captures

  Log(format('Adding new local %s, %s. Current local count : %d',[TokenKindToStr(Token.Kind),TokenName(Token),Current.Locals.Count]));
end;





procedure TCompilerController.declareLocalVariable();
var
  i: Integer;
  token: TToken; // Assuming TToken is a pointer to the Token struct.
  local : TLocal;
begin
  if Current.ScopeDepth = 0 then
    Exit;

  token := FTokens.previous;


  for i := Current.Locals.Count - 1 downto 0 do
  begin
    local := Current.Locals[i];
    if (local.depth <> -1) and (local.depth < Current.ScopeDepth) then
    begin
      Break;
    end;

    if identifiersEqual(token, Current.Locals[i].token) then
    begin
      error('Already a variable with this name in this scope.');
      exit;
    end;
  end;

  addLocal(token);
end;


procedure TCompilerController.varDeclaration;
var
  constantIdx : Integer;
begin

  constantIdx := parseVariable('Expect variable name.');


  if (match(tkequal)) then
  begin
    expression();
  end
  else
  begin
    Current.Func.Chunks.ADDNil;
  end;
  consume(tkSemiColon,'Expect ";" after variable declaration.');

  defineVariable(constantidx);
end;

Procedure TCompilerController.beginScope;
begin
  Current.ScopeDepth := Current.ScopeDepth + 1;
  Log(format('Begin Scope : %d',[Current.ScopeDepth]));
end;


procedure TCompilerController.block();
begin
  Log(format('Begin block at : %d',[Current.ScopeDepth]));
  while (not checkKind(tkCloseBrace) and not checkKind(tkEOF)) do
  begin
    declaration();
  end;
  consume(tkCloseBrace, 'Expect "}" after block.');
  Log(format('end block at : %d',[Current.ScopeDepth]));
end;


procedure TCompilerController.endScope();
var
  Local : TLocal;
  i : integer;
  scope : string;
begin
  Log(format('End scope : %d',[Current.ScopeDepth]));
  Current.ScopeDepth := Current.ScopeDepth -1;
  removeLocal;   //current->localCount--;
end;

procedure TCompilerController.PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  // -2 to adjust for the bytecode for the jump offset itself.
  Jump := Current.Func.Chunks.Count - OffSet - 2;

  if Jump > MAX_JUMP then
    Error('Too much code to jump over.');

  Current.Func.Chunks[OffSet]   := (Jump shr 8) and $FF;
  Current.Func.Chunks[OffSet+1] := Jump and $FF;
end;

procedure TCompilerController.LocalsToString(const strings: TStrings);
var
  Compiler : TCompiler;
  i : integer;
begin
  for i := 0 to FCompilers.Count-1 do
  begin
     Compiler := FCompilers[i];
     strings.Add('Current Compiler : ' + Compiler.Name);
     Compiler.Locals.ToString(Strings);
  end;
end;

function TCompilerController.emitJump(const instruction : TOpCodes) : integer;
begin
  Current.Func.Chunks.Emit(instruction);
  Current.Func.Chunks.Emit($FF); //255
  Current.Func.Chunks.Emit($FF); //255
  result :=  Current.Func.Chunks.count - 2;
end;




procedure TCompilerController.ifStatement;
var
  thenJump,
  elseJump : integer;

begin
  consume(tkOpenBracket,'Expect "(" after "if".');

  expression();

  consume(tkCloseBracket, 'Expect ")" after condition.');

  thenJump := emitJump(OP_JUMP_IF_FALSE);
  //emitOp(OpCode.POP);
  Current.Func.Chunks.ADDPOP;

  statement();

  elseJump := emitJump(OP_JUMP);

  patchJump(thenJump);
  //emitOp(OpCode.POP);
  Current.Func.Chunks.ADDPOP;

  if (match(tkElse)) then statement();

  patchJump(elseJump);

end;


procedure TCompilerController.Subscript(const canAssign: Boolean);
begin
  ParsePrecedence(PREC_OR);
  Consume(tkCloseSquareBracket, 'Expect '']'' after index.');

  if canAssign and Match(tkEqual) then
  begin
    Expression;
    Current.Func.Chunks.Emit(OP_STORE_SUBSCR);
  end
  else
  begin
    Current.Func.Chunks.Emit(OP_INDEX_SUBSCR);
  end;
end;



procedure TCompilerController.ListInit(const canAssign: Boolean);
var
  ItemCount : integer;
begin
  itemCount := 0;
  
  if not CheckKind(tkCloseSquareBracket) then
  begin
    repeat
      if CheckKind(tkCloseSquareBracket)then
      begin
        // Trailing comma case
        Break;
      end;

      ParsePrecedence(PREC_OR);        

      if itemCount = 256 then
      begin
        Error('Cannot have more than 256 items in a list literal.');
      end;
      Inc(itemCount);
    until not Match(tkComma);
  end;

  Consume(tkCloseSquareBracket, 'Expect '']'' after list literal.');

  Current.Func.Chunks.Emit(OP_BUILD_LIST);
  Current.Func.Chunks.Emit(itemCount);
end;



procedure TCompilerController.emitLoop(const loopStart : integer);
var
  offset : integer;
begin
  Current.Func.Chunks.Emit(OP_LOOP);

  offset := Current.Func.Chunks.count - loopStart + 2;
  if (offset > MAX_JUMP) then error('Loop body too large.');

  Current.Func.Chunks.Emit((offset shr 8) and $ff);
  Current.Func.Chunks.Emit(offset and $ff);

end;
 
procedure TCompilerController.whileStatement();
var
  exitJump : integer;
  loopStart : integer;

begin
  
  loopStart := Current.Func.Chunks.count;
  consume(tkOpenBracket, 'Expect "(" after while.');

   
  expression();
  consume(tkCloseBracket, 'Expect ")" after condition.');
  exitJump := emitJump(OP_JUMP_IF_FALSE);
  Current.Func.Chunks.addPOP;
  statement();
  emitLoop(loopStart);
  patchJump(exitJump);
  Current.Func.Chunks.addPOP;

end;




procedure TCompilerController.returnStatement();
begin

  if (Current.Func.FuncKind = TYPE_SCRIPT) then
  begin
    raise exception.create('Cant return from top-level code.');
  end;

  if match(tkSemiColon) then    
  begin
    emitReturn;
    exit;
  end;

  expression;
  consume(tkSemiColon, 'Expect ";" after return value.');
  Current.Func.Chunks.AddReturn;
end;





procedure TCompilerController.statement;
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
 
  if (match(tkReturn)) then
  begin

    returnStatement();
    exit;
  end;

  expressionStatement;

end;

procedure TCompilerController.Error(const msg: String);
begin
  Showmessage(msg);
end;


function TCompilerController.checkKind(const Kind : TTokenKind) : boolean;
begin
  result := FTokens.Current.Kind = Kind;
end;

function TCompilerController.match(const Expected : TTokenKind) : boolean;
begin
  result := CheckKind(Expected);
  if not result then exit;
  advance;
end;



function TCompilerController.argumentList: Byte; //we never get to call currently with the limited expression we calculate...
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





procedure TCompilerController.Strings(const canAssign: Boolean);
var
  Value : pValueRecord;
  Token : TToken;
  Text  : String;
begin
  Token := FTokens.previous;
  text := TokenName(Token);
  Value := BorrowChecker.NewString(rCompiler,Text);
  Current.Func.Chunks.EmitConstant(Value);
end;



procedure TCompilerController.call(const canAssign : boolean);
var
  argCount : byte;
  count : Integer;
begin
  argCount := argumentList;
  Current.Func.Chunks.Emit(ord(OP_CALL), argCount);

end;

procedure TCompilerController.binary(const canAssign : boolean);
var
  TokenKind : TTokenKind;
  rule : pParseRule;
begin
  TokenKind := FTokens.Previous.Kind;
  rule := FParseRules[FTokens.Previous.Kind];
  parsePrecedence(TPrecedence(ord(rule.Precedence) + 1));


  Case (TokenKind) of
    TkPlus              : Current.Func.Chunks.AddADD;
    TkMinus             : Current.Func.Chunks.AddSUBTRACT;
    tkAsterisk          : Current.Func.Chunks.AddMULTIPLY;
    tkEqualEqual        : Current.Func.Chunks.AddEQUAL;
    tkBangEqual         : Current.Func.Chunks.AddNOTEqual;
    tkLessThan          : Current.Func.Chunks.AddLess;
    tkLessThanEqual     : Current.Func.Chunks.AddLESSTHANEQUAL;
    tkGreaterThanEqual  : Current.Func.Chunks.AddGREATERTHANEQUAL;
    tkgreaterthan       : Current.Func.Chunks.AddGREATER;
    tkSlash             : Current.Func.Chunks.AddDivide;
    else
    begin
      raise exception.create('A token kind not catered for in a binary operation was encountered');
    end;
  end


end;

//virtual machine calls compile from interpret...
function TCompilerController.DoCompile : pLoxFunction;
begin
  advance;
  while not Match(tkEOF) do
  begin
    Declaration;
  end;

  result := Current.Func;
end;

 
procedure TCompilerController.EmitReturn;
begin
  Current.Func.Chunks.Emit(byte(OP_NIL));
  Current.Func.Chunks.Emit(byte(OP_RETURN));
end;



function TCompilerController.EndCompiler: pLoxFunction;
var
  fn : pLoxFunction;
begin
  EmitReturn;
  fn := Current.Func;
  current := current.enclosing;
  result := fn;
end;





 procedure TCompilerController.DoFunction (FunctionKind : TFunctionKind);
 var
   compiler: TCompiler;
   functionObj: pLoxFunction;
   value : pValueRecord;
   constantIdx : integer;
   name : string;
begin
   name := TokenName(FTokens.previous);

   Compiler := FCompilers.Add(Name,FunctionKind,Current);

   Current := Compiler;


   BeginScope;
   Consume(tkOpenBracket,    'Expect ''('' after function name.');

   if not CheckKind(tkCloseBracket) then
   begin
      repeat
        Compiler.Func.arity := Compiler.FFunc.arity + 1;
        if Compiler.Func.arity > 255 then
        begin
          raise exception.create('Cant have more than 255 parameters.');
        end;
        constantIdx :=  ParseVariable('Expect parameter name.');
        DefineVariable(constantIdx);
    until not Match(tkComma);
  end;

   Consume(tkCloseBracket,   'Expect '')'' after parameters.');
   Consume(tkOpenBrace, 'Expect ''{'' before function body.');
   Block;

   functionObj := EndCompiler; //in the c version this makes current to previous one.

   Value := BorrowChecker.newValueFromFunction(functionObj);

   Current.Func.Chunks.EmitConstant(Value);

end;


procedure TCompilerController.funDeclaration;
var
  globalIdx : integer;
begin
  globalIdx := parseVariable('Expect function name.');
  markInitialized();
  DoFunction(TYPE_FUNCTION);
  defineVariable(globalIdx);
end;



function TCompilerController.getCompiler(const index: integer): TCompiler;
begin
  result := FCompilers[index];
end;

function TCompilerController.getCount: integer;
begin
  result := FCompilers.Count;
end;

procedure TCompilerController.declaration;
begin
  if match(tkfun) then
  begin
     funDeclaration;
     exit;
  end;

  if match(tkVar) then
  begin
     varDeclaration;
     exit;
  end;

  statement();
end;


procedure TCompilerController.SetRuleForOpen_bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Prefix := grouping;
  ParseRule.Infix := call;
  ParseRule.Precedence := PREC_CALL;
  FParseRules[tkOpenbracket] := ParseRule;
end;


procedure TCompilerController.SetRulesForOpenBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkOpenBrace] := ParseRule;
end;

procedure TCompilerController.SetRulesForCloseBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkCloseBrace] := ParseRule;
end;

procedure TCompilerController.SetRulesForClose_Bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkCloseBracket] := ParseRule;
end;

procedure TCompilerController.SetRulesForBang;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE}
  ParseRule.Prefix := unary;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkBang] := ParseRule;
end;

procedure TCompilerController.SetRulesForComma;
var
  parseRule : pParseRule;
begin
  //  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  new(parseRule);

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkComma] := ParseRule;
end; 

procedure TCompilerController.SetRulesForComment;
var
  parseRule : pParseRule;
begin
  new(parseRule);

  ParseRule .Prefix := literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkComment] := ParseRule;
end;

procedure TCompilerController.SetRulesForDivide;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},

  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_FACTOR;
  FParseRules[tkSlash] := ParseRule;
end;

procedure TCompilerController.SetRulesForNil;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},

  ParseRule .Prefix := literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkNil] := ParseRule;
end;

procedure TCompilerController.SetRulesForNotEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},

  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_EQUALITY;
  FParseRules[tkBangEqual] := ParseRule;
end;

procedure TCompilerController.SetRulesForNumber;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_NUMBER]   = {number,   NULL,   PREC_NONE},

  ParseRule .Prefix := number;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkNumber] := ParseRule;
end;


procedure TCompilerController.SetRulesForEqualEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},

  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_EQUALITY;
  FParseRules[tkEqualEqual] := ParseRule;
end;


procedure TCompilerController.SetRulesForFalse;
var
  parseRule : pParseRule;
begin
  new(parseRule);
 //[TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE}

  ParseRule .Prefix := literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkFalse] := ParseRule;
end;

procedure TCompilerController.SetRulesForFun;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //   [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkFun] := ParseRule;


end;

procedure TCompilerController.SetRulesForTrue;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_TRUE] = {literal,  NULL,   PREC_NONE}

  ParseRule .Prefix := literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkTrue] := ParseRule;
end;

procedure TCompilerController.SetRulesForWhile;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkWhile] := ParseRule;
end;

procedure TCompilerController.SetRulesForGreaterThan;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON}

  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_COMPARISON;
  FParseRules[tkGreaterthan] := ParseRule;
end;

procedure TCompilerController.SetRulesForGreaterThanEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},


  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_COMPARISON;
  FParseRules[tkGreaterthanequal] := ParseRule;
end;

procedure TCompilerController.SetRulesForIdentifier;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//   [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE}

  ParseRule.Prefix := variable;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkIdentifier] := ParseRule;
end;

procedure TCompilerController.SetRulesForIF;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},

  ParseRule.Prefix := nil;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkIf] := ParseRule;
end;

procedure TCompilerController.SetRulesForLessThan;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},

  ParseRule.Prefix := nil;
  ParseRule.Infix := binary;
  ParseRule.Precedence := PREC_COMPARISON;
  FParseRules[tkLessThan] := ParseRule;
end;

procedure TCompilerController.SetRulesForLessThanEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},

  ParseRule.Prefix := nil;
  ParseRule.Infix := binary;
  ParseRule.Precedence := PREC_COMPARISON;
  FParseRules[tkLessThanEqual] := ParseRule;
end;

procedure TCompilerController.SetRulesForPlus;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},


  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_TERM;
  FParseRules[tkPlus] := ParseRule;
end;


procedure TCompilerController.SetRulesForPrint;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE}

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkPrint] := ParseRule;
end;


procedure TCompilerController.SetRulesForSemiColon;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE}

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkSemiColon] := ParseRule;
end;

procedure TCompilerController.SetRulesForString;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_STRING]        = {string,   NULL,   PREC_NONE},

  ParseRule .Prefix := strings;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkQuotes] := ParseRule;
end;

procedure TCompilerController.SetRulesForMinus;
var
  parseRule : pParseRule;
begin
  new(parseRule);
 // [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},


  ParseRule .Prefix := unary;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_TERM;
  FParseRules[tkMinus] := ParseRule;
end;

procedure TCompilerController.SetRulesForMultiply;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  ParseRule .Prefix := nil;
  ParseRule .Infix := binary;
  ParseRule .Precedence := PREC_FACTOR;
  FParseRules[tkAsterisk] := ParseRule;
end;

procedure TCompilerController.SetRulesForElse;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkElse] := ParseRule;
end;

procedure TCompilerController.SetRulesForEOF;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},


  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkEof] := ParseRule;
end;

procedure TCompilerController.SetRulesForOR;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_OR]            = {NULL,     or_,    PREC_OR},

  ParseRule .Prefix := nil;
  ParseRule .Infix := Or_;
  ParseRule .Precedence := PREC_OR;
  FParseRules[tkOr] := ParseRule;
end;

procedure TCompilerController.SetRulesForAnd;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_AND]           = {NULL,     and_,   PREC_AND},

  ParseRule .Prefix := nil;
  ParseRule.Infix := and_;
  ParseRule.Precedence := PREC_AND;
  FParseRules[tkAnd] := ParseRule;
end;


procedure TCompilerController.SetRules ;
begin
  SetRuleForOpen_bracket;
  SetRulesForClose_bracket;
  SetRulesForOpenBrace;
  SetRulesForCloseBrace;
  SetRulesForNumber;
  SetRulesForEqualEqual;
  SetRulesForNotEqual;
  SetRulesForLessThan;
  SetRulesForLessThanEqual;
  SetRulesForGreaterThan;
  SetRulesForGreaterThanEqual;
  SetRulesForPlus;
  SetRulesForMinus;
  SetRulesForMultiply;
  SetRulesForDivide;
  SetRulesForFalse;
  SetRulesForTrue;
  SetRulesForNil;
  SetRulesForBang;
  SetRulesForComment;
  SetRulesForString;
  SetRulesForSemiColon;
  SetRulesForPrint;
  SetRulesForIdentifier;
  SetRulesForIF;
  SetRulesForElse;
  SetRulesForAnd;
  SetRulesForOr;
  SetRulesForComma;
  SetRulesForWhile;
  SetRulesForEOF;
  SetRulesForFun;
  SetRulesForOpenSquareBracket;
  SetRulesForCloseSquareBracket;

end;



constructor TCompilerController.Create(
  const Tokens  : TTokenIterator;
  const Scanner : TScanner;
  const FunctionKind : TFunctionKind);

var
  Token : TToken;
begin
  FCompilers := TCompilers.Create;

  Current := FCompilers.add('toplevelcompiler',FunctionKind,nil);

  SetRules;

  Assert(Scanner.TokenCount > 1, 'No text to compile');   //it should have at least 1. (regardless of text scanned, as it always adds 1 extra EOF_TOKEN)

  FScanner := Scanner;

  FTokens := Tokens;

end;



destructor TCompilerController.destroy;
var
  i : TTokenKind;
begin
  for i := low(TTokenKind) to high(TTokenKind) do
  begin
    if FParseRules[i] <> nil then
      dispose(FParseRules[i]);
  end;
  //disposeFunction(FFunc);
  //FLocals.Finalize;
  FCompilers.free;
  inherited;
end;


{ TCompilers }

function TCompilers.Add(
  const Name : string;
  const FunctionKind : TFunctionKind;
  const Enclosing    : TCompiler): TCompiler;
begin
   result := TCompiler.Create(Name,FunctionKind,Enclosing);
   FItems.Add(result);
end;

constructor TCompilers.create;
begin
  FItems := TList.create;
end;

destructor TCompilers.destroy;
var
  i : integer;
  c : Tcompiler;
  fn : pLoxFunction;
begin
  if FItems.Count > 0 then
  begin
    c := FItems[0];
    fn := c.Func;
    BorrowChecker.Dispose(fn);
    for i := FItems.Count-1 downto 0 do
    begin
      TCompiler(FItems[i]).free;
    end;
  end;

  FItems.Free;
  inherited;
end;

function TCompilers.getCompiler(
  const Index: integer): TCompiler;
begin
  result := TCompiler(FItems[index]);
end;

function TCompilers.getCOunt: integer;
begin
  result := FItems.Count;
end;

function TCompilers.GetCurrent: TCompiler;
begin
  result := FItems[FItems.Count-1]; //start with premise it's the last one on the stack
end;

procedure TCompilers.setCompiler(
  const Index: integer;
  const Value: TCompiler);
begin
  FItems[Index] := Value;
end;

{ TCompiler }



function TCompiler.GetLocals : TLocals;
begin
  result := FLocals;
end;


procedure TCompiler.ToString(const strings: TStrings);
var
  opCode : TOpCodes;
  Chunks : TChunks;
  InstructionPointer : TInstructionPointer;
  ArgCount : integer;
  idx : integer;
  value : pValueRecord;
begin
  assert(assigned(strings),'No strings to print to');
  assert(assigned(Func), 'no function in compiler');
  assert(assigned(Func.Chunks),'no chunks in compiler');
  Chunks := Func.Chunks;

  InstructionPointer := TInstructionPointer.create ;
  InstructionPointer.Func := Func;
  strings.Add(Func.Name);
  while (InstructionPointer.Next <> -1) do
  begin
    Case TOpCodes(InstructionPointer.Current) of

      OP_CONSTANT : begin
         opCode := TOpCodes(InstructionPointer.Current);
         idx := InstructionPointer.Next; //skip index
         value := Func.Chunks.Constant[idx];
         strings.Add(OpCodeToStr(opCode) + ',' + inttostr(idx) + ', value : ' + GetString(value));

      end;


      OP_DEFINE_GLOBAL: begin
          strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.Current)));
          InstructionPointer.Next;
       end;

      OP_POP : Begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.Current)));
      end;


      OP_SET_GLOBAL:
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.Current)));
         InstructionPointer.Next;
      end;



      OP_GET_GLOBAL:
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.Current )));
         InstructionPointer.Next;
      end;


      OP_GET_LOCAL:
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.Current )));
          InstructionPointer.Next;

      end;

      OP_SET_LOCAL:
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
         InstructionPointer.Next;
      end;


      OP_Nil  : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_TRUE : begin
          strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_FALSE : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_GREATER : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_LESS : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));

      end;

      OP_EQUAL : begin
          strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_NOT : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;


      OP_ADD : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_SUBTRACT : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));

      end;


      OP_DIVIDE : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_MULTIPLY : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_NEGATE : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

      OP_PRINT  : begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;


      OP_JUMP_IF_FALSE:

      begin
        strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
        InstructionPointer.Next;
        InstructionPointer.Next;
      end;


      OP_JUMP:
      begin
        strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
         InstructionPointer.Next;
        InstructionPointer.Next;
      end;


       OP_LOOP:
       begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
         InstructionPointer.Next;
         InstructionPointer.Next;
       end;

      OP_CALL :
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
         ArgCount := InstructionPointer.Next;
      end;


      OP_Return :
      begin
         strings.Add(OpCodeToStr(TOpCodes(InstructionPointer.current)));
      end;

    end;


  end;
end;

constructor TCompiler.Create(
  const Name : string;
  const FunctionKind : TFunctionKind;
  const Enclosing    : TCompiler);

begin
  FLocals := TLocals.Create;
  FEnclosing  := Enclosing;
  FScopeDepth := 0;
  //FLocalCount := 0;
  FFunctionKind := FunctionKind;
  FName := Name;
  FFunc := BorrowChecker.newLoxFunction(FName);

  FInternal := TToken.Create;
  FInternal.Kind := tkNull;
  Locals.Add('',FInternal); //add an empty local for later use internally by the VM.   *)
end;

destructor TCompiler.destroy;
begin                
  FInternal.free;
  Flocals.Free;
  inherited;
end;

end.

