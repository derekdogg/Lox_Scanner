unit compiler;

interface

uses
  dialogs,Classes,LOXTypes, TokenArray, NewScanner, Locals, Values, OpCodes;


type

  TParseFn = procedure(const canAssign : boolean) of object;

  TParseRule = record
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkEOF] of TParseRule;

  TCompiler = class
  private
    FInternal       : TToken;
    FName           : String;
    FEnclosing      : TCompiler;
    Flocals         : TLocals;
    FFunc           : pLoxFunction; //dumb? why is this a pLoxfunction and not simply a TLoxFunction? More pointer indirection here
    FFunctionKind   : TFunctionKind;
    FScopeDepth     : integer;
  protected
    function GetLocals : TLocals;
  public

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


  //dumb - there is something fucked up with this, but I'm too tired and time-short to fix it right now.
  //why have a list of compilers when all you really care about is a list PloxFunctions, at best? Right?

  //probably, a stack of function makes more sense here.
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
    FStop : boolean;
    FCompilers : TCompilers;
    FCurrent : TCompiler;
    FParseRules : TParseRules;
    FTokenIterator : TTokenIterator;
//    FScanner : TScanner;

  protected

    //---------------------Compiler rules --------------------------------------
    procedure  SetRulesForOpenSquareBracket;
    procedure  SetRulesForCloseSquareBracket;
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


    //----------CHUNKS---------------------------------------------------------
    procedure Emit(const Operand : Integer; const  value : Integer);overload;
    procedure Emit(const Operand : Integer);overload;

    procedure EmitConstant(const value : TValueRecord);
    function EndCompiler: pLoxFunction;


    procedure markInitialized;
    procedure funDeclaration;
    procedure patchJump(const offset : integer);
    function emitJump(const instruction : Integer) : integer;
    procedure ifStatement;


    function  checkKind(const Kind : TTokenKind) : boolean;
    function  match(const Expected : TTokenKind) : boolean;
    procedure whileStatement;
    procedure returnStatement;
    procedure emitLoop(const loopStart : integer);

    Function resolveLocal(
      const Compiler : TCompiler;
      const Token : TToken) : integer;
    procedure AddLocal(const Token: TToken);
    function  identifiersEqual(const a, b: TToken): Boolean;
    procedure declareLocalVariable;
    procedure Error(Const msg : String);
    function argumentList: Byte;
    procedure NamedVariable(const Token : TToken; const CanAssign : Boolean);
    function advance : boolean;
    procedure printStatement;
    procedure ExpressionStatement;
    Procedure beginScope;
    procedure block;
    procedure endScope;
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
    procedure expression;
    function parseVariable(const errorMessage : string) : integer;
    procedure defineVariable(const constantidx : integer);
    procedure parsePrecedence(precedence : TPrecedence);
    procedure DoFunction (FunctionKind : TFunctionKind);
    procedure consume(const TokenKind : TTokenKind; const Message : String);
  public

    function DoCompile : pLoxFunction;

    constructor Create(
       const TokenIterator  : TTokenIterator;
//       const Scanner : TScanner;
       const FunctionKind : TFunctionKind);

    destructor destroy;override;

    property Stop : Boolean read FStop;


  end;



implementation

uses sysutils, valueManager;


function TCompilerController.advance : boolean;
begin
  result := false;
  if FStop then exit;
  result := FTokenIterator.MoveNext <> -1;
end;

procedure TCompilerController.Number(const canAssign : boolean);
var
  token : TToken;
  text : string;
  Value : TValueRecord;
begin
  if FStop then exit;

  Token := FTokenIterator.Previous;

  text := Token.Text;

  Value := bc.NewNumber(strToFloat(text));

  EmitConstant(Value);
end;



procedure TCompilerController.grouping(const canAssign : boolean);
begin
   if FStop then exit;
   expression;
   consume(tkclosebracket, 'Expect '')'' after expression.');
end;

procedure TCompilerController.literal(const CanAssign: boolean);
begin
  if FStop then exit;
  case  FTokenIterator.Previous.Kind of
    tkFalse : begin
       Emit(OP_FALSE);
    end;
    tknil   : begin
       Emit(OP_NIL);
    end;
    tkTrue  : begin
       Emit(OP_TRUE);
    end;
  end;
end;

procedure TCompilerController.Unary(const canAssign: boolean);
var
  TokenKind : TTokenKind;
begin
  if FStop then exit;
  TokenKind := FTokenIterator.previous.Kind;
  parsePrecedence(PREC_UNARY);
  case TokenKind of
    tkMinus : Emit(OP_NEGATE);
    tkBang  : Emit(OP_NOT);
  end;
end;

procedure TCompilerController.expression;
begin
  if FStop then exit;
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure TCompilerController.consume(const TokenKind : TTokenKind; const Message : String);
begin
  if FStop then exit;
  if FTokenIterator.Current.Kind = TokenKind then
  begin
    Advance;
    Exit;
  end;
  error(Message);
end;



//note book suggests better ways to do this, but this uses existing functionality.
Procedure TCompilerController.or_(const canAssign : boolean);
var
  elseJump : integer;
  endJump  : integer;
begin
  if FStop then exit;

  elseJump := emitJump(OP_JUMP_IF_FALSE);
  endJump := emitJump(OP_JUMP);

  if (elseJump = -1) or (endJump = -1) then
  begin
    FStop := true;
    exit;
  end;


  patchJump(elseJump);
  Emit(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
end;

Procedure TCompilerController.and_(const canAssign : boolean);
var
  endJump : integer;
begin
  if FStop then exit;
  endJump := emitJump(OP_JUMP_IF_FALSE);
  Emit(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
end;


procedure TCompilerController.printStatement;
begin
  if FStop then exit;
  expression;
  consume(tkSemiColon, 'Expect ";" after value.');
  Emit(OP_PRINT);
end;

procedure TCompilerController.ExpressionStatement;
begin
  if FStop then exit;
  Expression;
  Consume(tkSemiColon, 'Expect ";" after value.');
  Emit(OP_POP);
end;



function TCompilerController.resolveLocal(
  const Compiler : TCompiler;
  const Token : TToken) : integer;
var
  i : integer;
  local : TLocal;

  a : string;

begin
  if FStop then exit;
  result := -1;

  a := Token.Text;

  for i := Compiler.Locals.Count-1 downto 0 do
  begin
    Local := Compiler.locals[i];

    if identifiersEqual(Token, local.Token) then
    begin
      if (local.depth = -1) then
      begin
        error('Can''t read local variable in its own initializer.');
      end;

      result := i;
      exit;
    end;
  end;
  
end;



procedure TCompilerController.NamedVariable(const Token : TToken;const CanAssign : Boolean);
var
   getOp,setOp : Integer;
   Idx : integer;
   Value : TValueRecord;
begin
  if FStop then exit;

  Idx := resolveLocal(FCurrent,Token);

  if (Idx <> -1) then
  begin
    getOp := OP_GET_LOCAL;
    setOp := OP_SET_LOCAL;
  end
  else
  begin
    Value := bc.NewString(token.Text);
    idx :=   FCurrent.Func.AddConstant(Value);
    getOp := OP_GET_GLOBAL;
    setOp := OP_SET_GLOBAL;
  end;
 
  if canAssign and (match(tkEqual)) then
  begin
     expression;
     Emit(setOp, Idx);
   end
   else
   begin
     Emit(getOp, Idx);
   end;
end;

Procedure TCompilerController.variable(const CanAssign : boolean);
begin
  if FStop then exit;
  namedVariable(FTokenIterator.previous, CanAssign);
end;

procedure TCompilerController.markInitialized;
var
  Last : TLocal;
begin
  if FStop then exit;
  if FCurrent.ScopeDepth = 0 then exit;

  Last := FCurrent.Locals.Last;
  Last.Depth := FCurrent.ScopeDepth;
  FCurrent.Locals[FCurrent.Locals.Count-1] := Last;

//  FCurrent.Locals.Last.Depth := FCurrent.ScopeDepth;
end;

procedure TCompilerController.defineVariable(const constantIdx : integer);
begin
  if FStop then exit;
  if (FCurrent.scopeDepth > 0) then  // If we're in a nested scope, mark the variable as initialized locally.
  begin
    MarkInitialized;
    exit;
  end;
  Emit(OP_DEFINE_GLOBAL,ConstantIdx);
end;

procedure TCompilerController.parsePrecedence(precedence : TPrecedence);
var
  prefixRule : TParseFn;
  infixRule : TParseFn;
  canAssign : boolean;
begin
  if FStop then exit;

  if not advance then
  begin
    error('Advance Called when no further tokens' + TTokenName[FTokenIterator.Current.kind]);
    exit;
  end;

  prefixRule := FParseRules[FTokenIterator.Previous.kind].prefix;
  if (@prefixRule = nil) then
  begin
    error('Expected expression. i.e. no prefix rule, when expected one: ' + TTokenName[FTokenIterator.Previous.kind]);
    exit;
  end;

  canAssign := precedence <= PREC_ASSIGNMENT;

  prefixRule(canAssign);

  while (precedence <= FParseRules[FTokenIterator.current.Kind].precedence) do
  begin
    if not advance then exit;
    infixRule := FParseRules[FTokenIterator.previous.Kind].infix;
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


function TCompilerController.parseVariable(const errorMessage : string) : integer;
var
  Value : TValueRecord;
  Name  : String;
begin
  if FStop then exit;
  consume(tkIdentifier, errorMessage);

  if FCurrent.ScopeDepth > 0 then
  begin
    declareLocalVariable;
    result := 0;
    exit;
  end;

  Value  := bc.NewString(FTokenIterator.previous.Text);
  result := FCurrent.Func.AddConstant(Value);
end;


function TCompilerController.identifiersEqual(const a, b: TToken): Boolean;
var
  txt1,txt2 : string;
begin
  result := false;

  if FStop then exit;

  if a.kind <> b.kind then exit;

  txt1 := a.Text;

  txt2 := b.Text;

  result := txt1 = txt2;
end;


procedure TCompilerController.RemoveLocal;
begin
  if FStop then exit;

  while (FCurrent.locals.Count > 0) and (FCurrent.Locals.Last.depth > FCurrent.scopeDepth) do
  begin
    Emit(OP_POP);
    FCurrent.Locals.Count := FCurrent.Locals.Count - 1;
  end;
end;

procedure TCompilerController.AddLocal(const Token: TToken);
begin
  if FStop then exit;

  FCurrent.Locals.Add(token);
end;

procedure TCompilerController.declareLocalVariable;
var
  i: Integer;
  token: TToken; // Assuming TToken is a pointer to the Token struct.
  local : TLocal;
begin
  if FStop then exit;
  if FCurrent.ScopeDepth = 0 then
    Exit;

  token := FTokenIterator.previous;


  for i := FCurrent.Locals.Count - 1 downto 0 do
  begin
    local := FCurrent.Locals[i];
    if (local.depth <> -1) and (local.depth < FCurrent.ScopeDepth) then
    begin
      Break;
    end;

    if identifiersEqual(token, FCurrent.Locals[i].token) then
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
  if FStop then exit;

  constantIdx := parseVariable('Expect variable name.');


  if (match(tkequal)) then
  begin
    expression;
  end
  else
  begin
    Emit(OP_NIL);
  end;
  consume(tkSemiColon,'Expect ";" after variable declaration.');

  defineVariable(constantidx);
end;

Procedure TCompilerController.beginScope;
begin
  if FStop then exit;
  FCurrent.ScopeDepth := FCurrent.ScopeDepth + 1;

end;


procedure TCompilerController.block;
begin
  if FStop then exit;
  while (FStop = false) and
        (not checkKind(tkCloseBrace) and
        (not checkKind(tkEOF))) do
  begin
    declaration;
  end;

  if not FStop then
    consume(tkCloseBrace, 'Expect "}" after block.');
end;


procedure TCompilerController.endScope;
begin
  if FStop then exit;
  FCurrent.ScopeDepth := FCurrent.ScopeDepth -1;
  removeLocal;
end;



procedure TCompilerController.PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  if FStop then exit;

  assert(Offset >= 0, 'patch jump offset is < 0');


  Jump := FCurrent.Func.getCodeCount-1 - OffSet;

  if Jump > MAX_JUMP then
    Error('Too much code to jump over.');

  FCurrent.Func.OpCodes.Codes[OffSet] := Jump;

end;


function TCompilerController.emitJump(const instruction : Integer) : integer;
begin
  result := -1;
  if FStop then exit;
  Emit(Instruction);
  Emit($FF);
  result :=  FCurrent.Func.getCodecount - 1;
end;




procedure TCompilerController.ifStatement;
var
  thenJump,
  elseJump : integer;

begin
  if FStop then exit;
  consume(tkOpenBracket,'Expect "(" after "if".');

  expression;

  consume(tkCloseBracket, 'Expect ")" after condition.');

  thenJump := emitJump(OP_JUMP_IF_FALSE);

  Emit(OP_POP);

  statement;

  elseJump := emitJump(OP_JUMP);

  patchJump(thenJump);

  Emit(OP_POP);

  if (match(tkElse)) then statement;

  patchJump(elseJump);

end;


procedure TCompilerController.Subscript(const canAssign: Boolean);
begin
  if FStop then exit;
  ParsePrecedence(PREC_OR);
  Consume(tkCloseSquareBracket, 'Expect '']'' after index.');

  if canAssign and Match(tkEqual) then
  begin
    Expression;
    Emit(OP_STORE_SUBSCR);
  end
  else
  begin
    Emit(OP_INDEX_SUBSCR);
  end;
end;



procedure TCompilerController.ListInit(const canAssign: Boolean);
var
  ItemCount : integer;
begin
  if FStop then exit;
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

  Emit(OP_BUILD_LIST);
  Emit(itemCount);
end;



procedure TCompilerController.emitLoop(const loopStart : integer);
var
  offset : integer;
begin
  if FStop then exit;
  Emit(OP_LOOP);

  offset := FCurrent.Func.getCodeCount - loopStart + 1;
  if (offset > MAX_JUMP) then error('Loop body too large.');

  Emit(Offset);
  (*


  Emit((offset shr 8) and $ff);
  Emit(offset and $ff);
  *)
end;
 
procedure TCompilerController.whileStatement;
var
  exitJump : integer;
  loopStart : integer;
begin
  if FStop then exit;

  loopStart := FCurrent.Func.GetCodecount;

  consume(tkOpenBracket, 'Expect "(" after while.');

  expression;

  consume(tkCloseBracket, 'Expect ")" after condition.');

  exitJump := emitJump(OP_JUMP_IF_FALSE);

  Emit(OP_POP);

  statement;

  emitLoop(loopStart);

  patchJump(exitJump);

  Emit(OP_POP);

end;
 
procedure TCompilerController.returnStatement;
begin
  if FStop then exit;
  if (FCurrent.Func.FuncKind = TYPE_SCRIPT) then
  begin
    raise exception.create('Cant return from top-level code.');
  end;

  if match(tkSemiColon) then    
  begin
    Emit(OP_NIL);
    Emit(OP_RETURN);
    exit;
  end;

  expression;
  consume(tkSemiColon, 'Expect ";" after return value.');
  Emit(OP_Return);
end;





procedure TCompilerController.statement;
begin
  if FStop then exit;
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
    ifStatement;
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
    whileStatement;
    exit;
  end;
 
  if (match(tkReturn)) then
  begin

    returnStatement;
    exit;
  end;

  expressionStatement;

end;

//this pile of dogshit needs working on, boy.
procedure TCompilerController.Error(const msg: String);
begin
  FStop := true;
  Showmessage(msg);
end;


function TCompilerController.checkKind(const Kind : TTokenKind) : boolean;
begin
  result := false;
  if FStop then exit;
  result := FTokenIterator.Current.Kind = Kind;
end;

function TCompilerController.match(const Expected : TTokenKind) : boolean;
begin
  result := false;
  if FStop then exit;
  result := CheckKind(Expected);
  if not result then exit;
  advance;
end;



function TCompilerController.argumentList: Byte;
begin
  result := 0;
  if FStop then exit;
  if not (FTokenIterator.Current.Kind = tkcloseBracket) then
  begin
    repeat   //looking at the c code is this equivalent looping?
      expression;
      if result = 255 then
      begin
        error('Can''t have more than 255 arguments.');
      end;
      Inc(result);
    until not match(tkComma);
  end;
  consume(tkcloseBracket, 'Expect '')'' after arguments.');

end;


procedure TCompilerController.EmitConstant(const value : TValueRecord);
begin
  if FStop then exit;
  FCurrent.Func.EmitConstant(value);
end;


procedure TCompilerController.Strings(const canAssign: Boolean);
var
  Value : TValueRecord;
  Token : TToken;
  Text  : String;
begin
  if FStop then exit;
  Token := FTokenIterator.previous;
  text := Token.Text;
  Value := bc.NewString(Text);
  EmitConstant(Value);
end;



procedure TCompilerController.call(const canAssign : boolean);
var
  argCount : byte;
begin
  if FStop then exit;
  argCount := argumentList; 
  Emit(OP_CALL, argCount);
end;

procedure TCompilerController.binary(const canAssign : boolean);
var
  TokenKind : TTokenKind;
  rule : TParseRule;
begin
  if FStop then exit;
  TokenKind := FTokenIterator.Previous.Kind;
  rule := FParseRules[FTokenIterator.Previous.Kind];
  parsePrecedence(TPrecedence(ord(rule.Precedence) + 1));


  Case (TokenKind) of
    TkPlus : Emit(OP_ADD);

    TkMinus : Emit(OP_SUBTRACT);

    tkAsterisk : Emit(OP_MULTIPLY);

    tkEqualEqual : Emit(OP_EQUAL);

    tkBangEqual :
    begin
      Emit(OP_EQUAL);
      Emit(OP_NOT);
    end;

    tkLessThan          : Emit(OP_LESS);

    tkLessThanEqual     : begin
                           Emit(OP_GREATER);
                           Emit(OP_NOT);
    end;
    tkGreaterThanEqual  :
    begin
                            Emit(OP_LESS);
                            Emit(OP_NOT);
    end;

    tkgreaterthan       : Emit(OP_GREATER);

    tkSlash             : Emit(OP_DIVIDE);
    else
    begin
      raise exception.create('A token kind not catered for in a binary operation was encountered');
    end;
  end
end;

//virtual machine calls compile from interpret...
function TCompilerController.DoCompile : pLoxFunction;
begin
  FStop := false;

  advance;
  while (FStop = false) and not match(tkEof) do
  begin
    Declaration;
  end;


  result := FCurrent.Func;
end;


procedure TCompilerController.Emit(const Operand : Integer);
begin
  if FStop then exit;
  FCurrent.Func.Emit(Operand);
end;

 
function TCompilerController.EndCompiler: pLoxFunction;
var
  fn : pLoxFunction;
begin
  result := nil;
  if FStop then exit;
  Emit(OP_NIL);
  Emit(OP_RETURN);
  fn := FCurrent.Func;
  FCurrent := FCurrent.enclosing;
  result := fn;
end;



 procedure TCompilerController.DoFunction (FunctionKind : TFunctionKind);
 var
   compiler: TCompiler;
   functionObj: pLoxFunction;
   value : TValueRecord;
   constantIdx : integer;
   name : string;
begin
   if FStop then exit;
   name := FTokenIterator.previous.Text;

   Compiler := FCompilers.Add(Name,FunctionKind,FCurrent);

   FCurrent := Compiler;

   BeginScope;
   Consume(tkOpenBracket,    'Expect ''('' after function name.');

   if not CheckKind(tkCloseBracket) then
   begin
      repeat
        Compiler.Func.arity := Compiler.FFunc.arity + 1;
        if Compiler.Func.arity > 20 then
        begin
          raise exception.create('Cant have more than 20 parameters.');
        end;
        constantIdx :=  ParseVariable('Expect parameter name.');
        DefineVariable(constantIdx);
    until not Match(tkComma);
  end;

   Consume(tkCloseBracket,   'Expect '')'' after parameters.');
   Consume(tkOpenBrace, 'Expect ''{'' before function body.');
   Block;

   functionObj := EndCompiler; //in the c version this makes current to previous one.

   Value := bc.newValueFromFunction(functionObj);

   EmitConstant(Value);

end;



procedure TCompilerController.Emit(const Operand: Integer;
  const value: Integer);
begin
  if FStop then exit;
  FCurrent.Func.Emit(Operand,Value);
end;

procedure TCompilerController.funDeclaration;
var
  globalIdx : integer;
begin
  if FStop then exit;
  globalIdx := parseVariable('Expect function name.');
  markInitialized;
  DoFunction(TYPE_FUNCTION);
  defineVariable(globalIdx);
end;

procedure TCompilerController.declaration;
begin
  if FStop then exit;
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

  statement;
end;

procedure TCompilerController.SetRuleForOpen_bracket;
begin
 
  with FParseRules[tkOpenbracket] do
  begin
     Prefix := grouping;
     Infix := call;
     Precedence := PREC_CALL;
  end;
end;


procedure TCompilerController.SetRulesForOpenBrace;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForCloseBrace;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForClose_Bracket;
begin
   //nothing to set here
end;

procedure TCompilerController.SetRulesForBang;
begin
  with FParseRules[tkBang] do
  begin
      Prefix := unary;
   end;
end;

procedure TCompilerController.SetRulesForComma;
begin
   //nothing to set here
end; 

procedure TCompilerController.SetRulesForComment;
begin
  with FParseRules[tkComment] do
  begin
     Prefix := literal;
  end;
end;

procedure TCompilerController.SetRulesForDivide;
begin
  with FParseRules[tkSlash] do
  begin
     infix := binary;
     Precedence := PREC_FACTOR;
  end;
end;

procedure TCompilerController.SetRulesForNil;
begin
  with FParseRules[tkNil] do
  begin
    Prefix := literal;
  end;
end;

procedure TCompilerController.SetRulesForNotEqual;
begin
  with FParseRules[tkBangEqual] do
  begin
     Infix := binary;
     Precedence := PREC_EQUALITY;
  end;
end;

procedure TCompilerController.SetRulesForNumber;
begin
  with FParseRules[tkNumber] do
  begin
    Prefix := number;
  end;
end;


procedure TCompilerController.SetRulesForEqualEqual;
begin
  with FParseRules[tkEqualEqual] do
  begin
    Infix := binary;
    Precedence := PREC_EQUALITY;
  end;
end;


procedure TCompilerController.SetRulesForFalse;
begin
  with FParseRules[tkFalse] do
  begin
     Prefix := literal;
  end;
end;

procedure TCompilerController.SetRulesForFun;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForTrue;
begin
  with FParseRules[tkTrue] do
  begin
     Prefix := literal;
  end;
end;

procedure TCompilerController.SetRulesForWhile;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForGreaterThan;
begin
  with FParseRules[tkGreaterthan] do
  begin
    Infix := binary;
    Precedence := PREC_COMPARISON;
  end;
end;

procedure TCompilerController.SetRulesForGreaterThanEqual;
begin
  with FParseRules[tkGreaterthanequal] do
  begin
      Infix := binary;
      Precedence := PREC_COMPARISON;
  end;
end;

procedure TCompilerController.SetRulesForIdentifier;
begin
  with FParseRules[tkIdentifier] do
  begin
     Prefix := variable;
  end;
end;

procedure TCompilerController.SetRulesForIF;
begin
   //nothing to set here
end;

procedure TCompilerController.SetRulesForLessThan;
begin
  with FParseRules[tkLessThan] do
  begin
    Infix := binary;
    Precedence := PREC_COMPARISON;
  end;
end;

procedure TCompilerController.SetRulesForLessThanEqual;
begin
  with FParseRules[tkLessThanEqual] do
  begin
     Infix := binary;
     Precedence := PREC_COMPARISON;
  end;
end;

procedure TCompilerController.SetRulesForPlus;
begin
  with FParseRules[tkPlus] do
  begin
     Infix := binary;
     Precedence := PREC_TERM;
  end;
end;


procedure TCompilerController.SetRulesForPrint;
begin
  //nothing to set here
end;


procedure TCompilerController.SetRulesForSemiColon;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForString;
begin
  with FParseRules[tkQuotes] do
  begin
     Prefix := strings;
     Infix := binary;
  end;
end;

procedure TCompilerController.SetRulesForMinus;
begin

  with FParseRules[tkMinus] do
  begin
     Prefix := unary;
     Infix := binary;
     Precedence := PREC_TERM;
  end;
end;

procedure TCompilerController.SetRulesForMultiply;
begin
   with FParseRules[tkAsterisk] do
   begin
      Infix := binary;
      Precedence := PREC_FACTOR;
   end;
end;

procedure TCompilerController.SetRulesForElse;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForEOF;
begin
  //nothing to set here
end;

procedure TCompilerController.SetRulesForOR;
begin
  with FParseRules[tkOr] do
  begin
     Infix := Or_;
     Precedence := PREC_OR;
  end;
end;

procedure TCompilerController.SetRulesForAnd;
begin

  with FParseRules[tkAnd] do
  begin
     Infix := and_;
     Precedence := PREC_AND;
  end;
end;

procedure TCompilerController.SetRulesForOpenSquareBracket;
begin
  with FParseRules[tkOpenSquareBracket] do
  begin
     Prefix     :=  ListInit;
     Infix      :=  Subscript;
     Precedence :=  PREC_SUBScript;
  end;
end;

procedure TCompilerController.SetRulesForCloseSquareBracket;
begin
  //nothing to set here
end;


//this basically describes the lexical rules in place.
//so whilst some things are not actually needed, I keep them here for now
//it may be the case that we add a boolean to describe if a rule is in use.
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
  const TokenIterator  : TTokenIterator;
  const FunctionKind : TFunctionKind);

begin
  Assert(TokenIterator.Count > 1, 'No text to compile');   //it should have at least 1. (regardless of text scanned, as it always adds 1 extra EOF_TOKEN)

  FTokenIterator := TokenIterator;

  FCompilers := TCompilers.Create;

  FCurrent := FCompilers.add('toplevelcompiler',FunctionKind,nil);

  SetRules;


end;



destructor TCompilerController.destroy;
begin
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
    bc.Dispose(fn);
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
  FFunc := bc.newLoxFunction(FName);
  FInternal.Kind := tkNull;
  Locals.Add(FInternal); //add an empty local for later use internally by the VM.   *)
end;

destructor TCompiler.destroy;
begin

  Flocals.Free;
  inherited;
end;

end.

