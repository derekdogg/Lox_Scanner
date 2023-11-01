unit compiler;

interface

uses classes,LOXTypes, TokenArray, Scanner, locals,values, opCodes;


type

 TParseFn = procedure(const canAssign : boolean) of object;




  pParseRule = ^TParseRule;
  TParseRule = record
    prefix: TParseFn;
    infix: TParseFn;
    precedence: TPrecedence;
  end;

  TParseRules = array[tknull..tkEOF] of pParseRule;




  TCompiler = class;



  TCompiler = class
  private
    FGlobals : TValueList;   //scope wide
    FConstants : TValueList;

    FCurrent : TCompiler;

    FFunc : pLoxFunction;
    FFunctionKind : TFunctionKind;

    FParseRules : TParseRules;


    FScopeDepth : integer;
//    FLocalCount : integer;
    Flocals : pLocalList;
   // FChunks : TChunks;

    FTokens : pTokenIterator;
    FScanner : TScanner;
    //---------------------Compiler rules --------------------------------------
procedure  CreateRulesForOpenBrace;


procedure  CreateRulesForCloseBrace;


procedure  CreateRulesForClose_Bracket;


procedure  CreateRuleForOpen_bracket;



procedure  CreateRulesForBang;


procedure  CreateRulesForComment;


procedure  CreateRulesForDivide;


procedure  CreateRulesForNil;


procedure  CreateRulesForNotEqual;


procedure  CreateRulesForNumber;



procedure  CreateRulesForEqualEqual;



procedure  CreateRulesForFalse;


procedure  CreateRulesForTrue;


procedure  CreateRulesForWhile;


procedure  CreateRulesForGreaterThan;


procedure  CreateRulesForGreaterThanEqual;

procedure  CreateRulesForIdentifier;

procedure  CreateRulesForIF;


procedure  CreateRulesForLessThan;

procedure  CreateRulesForLessThanEqual;


procedure  CreateRulesForPlus;



procedure  CreateRulesForPrint;



procedure  CreateRulesForSemiColon;


procedure  CreateRulesForString;


procedure  CreateRulesForMinus;


procedure  CreateRulesForMultiply;


procedure  CreateRulesForElse;


procedure  CreateRulesForEOF;


procedure  CreateRulesForOR;


procedure  CreateRulesForAnd;

procedure CreateRulesForComma;


procedure CreateRules;


    //--------------------------------------------------------------------------
    procedure EmitReturn;
    function EndCompiler: pLoxFunction;
   procedure EmitConstant(const value : pValue);
    procedure markInitialized();
    procedure funDeclaration;
    procedure patchJump(const offset : integer);
    function emitJump(const instruction : TOpCodes) : integer;
    procedure ifStatement;
    function TokenName(const token : pToken) : String;
    procedure Log(const txt : String);

    function  checkKind(Kind : TTokenKind) : boolean;
    function  match(const Expected : TTokenKind) : boolean;
    procedure whileStatement();
    procedure returnStatement();
    procedure emitLoop(const loopStart : integer);

    Function resolveLocal(Token : pToken) : integer;
    procedure AddLocal(const Token: pToken);
    function  identifiersEqual(const a, b: PToken): Boolean;
    procedure declareLocalVariable();
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

    function declareGlobalVariable(const token : pToken) : integer;
    function parseVariable(const errorMessage : string) : integer;
    procedure defineVariable(const constantidx : integer);

    procedure parsePrecedence(precedence : TPrecedence);

    procedure DoFunction (FunctionKind : TFunctionKind);

    procedure consume(const TokenKind : TTokenKind; const Message : String);
    function GetParseRule(const TokenKind: TTokenKind): pParseRule;
    procedure SetParseRule(const TokenKind: TTokenKind;
      const Value: pParseRule);
  public
    


    function DoCompile : pLoxFunction;

   constructor Create(
       const Constants : TValueList;
       const Globals : TValueList;
       const Tokens  : pTokenIterator;
       const Scanner : TScanner;
       const FunctionKind : TFunctionKind);

    destructor destroy;override;


    property ParseRule[const TokenKind : TTokenKind] : pParseRule
      read   GetParseRule
      write  SetParseRule;
  end;



implementation

uses sysutils;


function TCompiler.advance : boolean;
begin
  //if FTokens.Current <> nil then
  //  log('Before Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));
  result := FTokens.MoveNext <> nil;

  //if FTokens.Current <> nil then
  //  log('After Advance. Current Token : ' + FScanner.TokenText(FTokens.Current^));

end;


// note : when this reaches here for the expression for the print statement, suddenly we are in the toplevel compilre fun
procedure TCompiler.EmitConstant(const value : pValue);
var
  constantIdx : integer;
begin
  constantIdx := FConstants.Add(value);
  FFunc.Chunks.EmitBytes(OP_CONSTANT,ConstantIdx);
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

  EmitConstant(Value);
end;



procedure TCompiler.grouping(const canAssign : boolean);
begin
   expression();
   consume(tkclosebracket, 'Expect '')'' after expression.');  //right bracket
end;

procedure TCompiler.literal(const CanAssign: boolean);
begin
  case  FTokens.Previous.Kind of
    tkFalse : FFunc.Chunks.AddFALSE;
    tknil   : FFunc.Chunks.AddNil;
    tkTrue  : FFunc.Chunks.AddTrue;
  end;
end;

procedure TCompiler.Unary(const canAssign: boolean);
var
  TokenKind : TTokenKind;
begin
  TokenKind := FTokens.previous.Kind;
  parsePrecedence(PREC_UNARY);
  case TokenKind of
    tkMinus : FFunc.Chunks.AddNEGATE;
    tkBang  : FFunc.Chunks.ADDNOT;
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

end;


//note book suggests better ways to do this, but this uses existing functionality.
Procedure TCompiler.or_(const canAssign : boolean);
var
  elseJump,endJump : integer;
begin
  elseJump := emitJump(OP_JUMP_IF_FALSE);
  endJump := emitJump(OP_JUMP);

  patchJump(elseJump);
  FFunc.Chunks.ADDPOP;

  parsePrecedence(PREC_OR);
  patchJump(endJump);
end;



Procedure TCompiler.and_(const canAssign : boolean);
var
  endJump : integer;
begin
  endJump := emitJump(OP_JUMP_IF_FALSE);
  FFunc.Chunks.AddPOP;
  parsePrecedence(PREC_AND);
  patchJump(endJump);
end;


procedure TCompiler.printStatement;
begin
  expression;
  consume(tkSemiColon, 'Expect ";" after value.');
  FFunc.Chunks.ADDPRINT;
end;

procedure TCompiler.ExpressionStatement;
begin
  Expression;
  Consume(tkSemiColon, 'Expect ";" after value.');
  FFunc.Chunks.ADDPOP;
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
    arg = declareGlobalVariable(&name);
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
    Local := Flocals^[i];
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
      Log(format('resolved local for %s. found at local index index %d',[TokenName(Token),i]));
      exit;
    end;
  end;
  Log(format('unable to resolve local for %s.',[TokenName(Token)]));
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
  begin //local not found in scope must be global


    Idx := declareGlobalVariable(Token); //index of global
    getOp := OP_GET_GLOBAL;
    setOp := OP_SET_GLOBAL;
  end;

  if canAssign and (match(tkEqual)) then
  begin
     expression();

     //FFunc.Chunks.AddSET_GLOBAL(global);
     FFunc.Chunks.emitBytes(byte(setOp), Idx);

   end
   else
   begin
     //FFunc.Chunks.AddGET_GLOBAL(global);

     FFunc.Chunks.emitBytes(byte(getOp), Idx);

   end;
end;

Procedure TCompiler.variable(const CanAssign : boolean);
begin
  namedVariable(FTokens.previous, CanAssign);
end;


procedure TCompiler.markInitialized();
begin
  if FScopeDepth = 0 then exit;
  FLocals^[FLocals.Count-1].Depth := FScopeDepth;

end; {

  if (current->scopeDepth == 0) return;

  current->locals[current->localCount - 1].depth =
      current->scopeDepth;
}





procedure TCompiler.defineVariable(const constantIdx : integer);
begin
  log('define variable');
  if (FscopeDepth > 0) then  // If we're in a nested scope, mark the variable as initialized locally.
  begin
    log('scope depth is > 0 therefore not global');
    MarkInitialized;
    exit;
  end;

  // If we're in the outermost scope, define the variable as a global.
  log('define global variable');
  FFunc.Chunks.AddDEFINE_GLOBAL(constantidx);
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


function TCompiler.declareGlobalVariable(const token : pToken) : integer;
var
  value : pValue;
  text : string;

begin
  assert(token <> nil,'Name is nil, cannot add identified constant');
  text := FScanner.ln.items[Token.Line].text;
  text := copy(text,token.Start,token.length);
  Value :=  NewString(Text);//StringValue(text);

  if FGlobals.IndexOf(Value) = -1 then
  begin
    result := FGlobals.Add(Value);
  end;
end;


function TCompiler.parseVariable(const errorMessage : string) : integer;
begin
  Log('Parse variable');
  consume(tkIdentifier, errorMessage);

  if FScopeDepth > 0 then
  begin
    declareLocalVariable;
    result := 0;
    exit;
  end;



  result := declareGlobalVariable(FTokens.previous); //index of the variable name in the constant table
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
  local.depth := -1 ; //declare undefined
  Local.isCaptured := False; //presumably for future captures
  FLocals.Add(Local);
  Log(format('Adding new local %s, %s. Current local count : %d',[TokenKindToStr(Token.Kind),TokenName(Token),FLocals.Count]));
end;

 



procedure TCompiler.declareLocalVariable();
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
    local := Flocals^[i];
    if (local.depth <> -1) and (local.depth < FScopeDepth) then
    begin
      Break;
    end;

    if identifiersEqual(token, Flocals^[i].token) then
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
    FFunc.Chunks.ADDNil;
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
    FFunc.Chunks.AddPOP;
    Local := FLocals.Remove(FLocals.Count-1);
    Dispose(Local);
    //Log(format('Remove local kind : %s, name : %s, local count : %d',[TokenKindToStr(Local.Token.Kind),TokenName(Local.Token),FLocals.Count]));
  end;
end;

procedure TCompiler.PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  // -2 to adjust for the bytecode for the jump offset itself.
  Jump := FFunc.Chunks.Count - OffSet - 2;

  if Jump > MAX_JUMP then
    Error('Too much code to jump over.');

  pbyte(FFunc.Chunks[OffSet])^   := (Jump shr 8) and $FF;
  pByte(FFunc.Chunks[OffSet+1])^ := Jump and $FF;
end;

function TCompiler.emitJump(const instruction : TOpCodes) : integer;
begin
  FFunc.Chunks.emitByte(Byte(instruction));
  FFunc.Chunks.emitByte($FF);
  FFunc.Chunks.emitByte($FF);
  result :=  FFunc.Chunks.count - 2;
end;


(*

  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "if".');

  if Match(TOKEN_VAR) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after condition.');

  ThenJump := EmitJump(op_Jump_If_False);
  EmitByte(op_Pop);
  Statement;

  ElseJump := EmitJump(op_Jump);
  PatchJump(ThenJump);
  EmitByte(op_Pop);

  if Match(TOKEN_ELSE) then
    Statement;
  PatchJump(ElseJump);

  if VarDeclared then
    EndScope;



*)

procedure TCompiler.ifStatement;
var
  thenJump,elseJump : integer;
  vardeclared : boolean;
begin
  consume(tkOpenBracket,'Expect "(" after "if".');

  if Match(tkvar) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  expression();
  consume(tkCloseBracket, 'Expect ")" after condition.');

  thenJump := emitJump(OP_JUMP_IF_FALSE);

  statement();

  elseJump := emitJump(OP_JUMP);
  patchJump(thenJump);

  if (match(tkElse)) then statement();

  patchJump(elseJump);

  if VarDeclared then
    EndScope;
end;



procedure TCompiler.emitLoop(const loopStart : integer);
var
  offset : integer;
begin
  FFunc.Chunks.emitByte(OP_LOOP);

  offset := FFunc.Chunks.count - loopStart + 2;
  if (offset > MAX_JUMP) then error('Loop body too large.');

  FFunc.Chunks.emitByte((offset shr 8) and $ff);
  FFunc.Chunks.emitByte(offset and $ff);

end;

(*LoopStart := CurrentChunk^.Count;

  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "while".');

  if Match(TOKEN_VAR) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after condition.');

  ExitJump := EmitJump(op_Jump_If_False);

  EmitByte(op_Pop);

  Statement;

  EmitLoop(LoopStart);

  PatchJump(ExitJump);
  EmitByte(op_Pop);

  if VarDeclared then
    EndScope;*)

procedure TCompiler.whileStatement();
var
  exitJump : integer;
  loopStart : integer;
  vardeclared : boolean;
begin
  
  loopStart := FFunc.Chunks.count;
  consume(tkOpenBracket, 'Expect "(" after while.');
  if Match(tkVar) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  expression();
  consume(tkCloseBracket, 'Expect ")" after condition.');
  exitJump := emitJump(OP_JUMP_IF_FALSE);
  FFunc.Chunks.addPOP;
  statement();
  emitLoop(loopStart);
  patchJump(exitJump);
  FFunc.Chunks.addPOP;
  if VarDeclared then
    EndScope
end;




procedure TCompiler.returnStatement();
begin
  
  if (FFunc.FuncKind = TYPE_SCRIPT) then
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
  FFunc.Chunks.AddReturn;
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


  if (match(tkReturn)) then
  begin
    
    returnStatement();
    exit;
  end;


  expressionStatement;


End;

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
  EmitConstant(Value);
end;



procedure TCompiler.call(const canAssign : boolean);
var
  argCount : byte;
begin
  argCount := argumentList;
  FFunc.Chunks.emitBytes(ord(OP_CALL), argCount);
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
    TkPlus              : FFunc.Chunks.AddADD;
    TkMinus             : FFunc.Chunks.AddSUBTRACT;
    tkAsterisk          : FFunc.Chunks.AddMULTIPLY;
    tkEqualEqual        : FFunc.Chunks.AddEQUAL;
    tkBangEqual         : FFunc.Chunks.AddNOTEqual;
    tkLessThan          : FFunc.Chunks.AddLess;
    tkLessThanEqual     : FFunc.Chunks.AddLESSTHANEQUAL;
    tkGreaterThanEqual  : FFunc.Chunks.AddGREATERTHANEQUAL;
    tkgreaterthan       : FFunc.Chunks.AddGREATER;
    tkSlash             : FFunc.Chunks.AddDivide;
    else
    begin
      raise exception.create('A token kind not catered for in a binary operation was encountered');
    end;
  end


end;

//virtual machine calls compile from interpret...
function TCompiler.DoCompile : pLoxFunction;
begin
  advance;
  while not Match(tkEOF) do
  begin
    Declaration;
  end;

  result := FFunc;
end;

 
procedure TCompiler.EmitReturn;
begin
  FFunc.Chunks.emitByte(byte(OP_NIL));
  FFunc.Chunks.emitByte(byte(OP_RETURN));
end;



function TCompiler.EndCompiler: pLoxFunction;
begin
  EmitReturn;
  Result := FFunc;
 // current := current^.enclosing; we don't need to do this, I think, because it is already nested
end;



 procedure TCompiler.DoFunction (FunctionKind : TFunctionKind);
 var
   compiler: TCompiler;
   functionObj: pLoxFunction;
   value : pValue;
   constantIdx : integer;
begin
 
   Compiler := TCompiler.Create(FConstants,FGlobals,FTokens,FScanner,FunctionKind);
    
   Compiler.FScopeDepth := FScopeDepth;
   Compiler.FLocals := FLocals;   //this will mem leak currently
   Compiler.BeginScope;
   Compiler.Consume(tkOpenBracket,    'Expect ''('' after function name.');

   if not CheckKind(tkCloseBracket) then
   begin
      repeat
        Compiler.FFunc.arity := Compiler.FFunc.arity + 1;
        if Compiler.FFunc.arity > 255 then
        begin
          raise exception.create('Cant have more than 255 parameters.');
        end;
        constantIdx := Compiler.ParseVariable('Expect parameter name.');
        Compiler.DefineVariable(constantIdx);
    until not Match(tkComma);
  end;





   Compiler.Consume(tkCloseBracket,   'Expect '')'' after parameters.');
   Compiler.Consume(tkOpenBrace, 'Expect ''{'' before function body.');
   Compiler.Block;

   functionObj := Compiler.EndCompiler; //in the c version this makes current to previous one.

   //EmitBytes(OP_CONSTANT, MakeConstant(OBJ_VAL(functionObj)));

   Value := newValueFromFunction(functionObj);
   //ConstantIdx := FFunc.Chunks.MakeConstant(Value);

   //FFunc.Chunks.emitBytes(byte(OP_CONSTANT), ConstantIdx);
   EmitConstant(Value);

end;


procedure TCompiler.funDeclaration;
var
  globalIdx : integer;
begin
  globalIdx := parseVariable('Expect function name.');
  markInitialized();
  DoFunction(TYPE_FUNCTION);
  defineVariable(globalIdx);
end;



procedure TCompiler.declaration;
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


procedure TCompiler.CreateRuleForOpen_bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Prefix := grouping;
  ParseRule.Infix := call;
  ParseRule.Precedence := PREC_CALL;
  FParseRules[tkOpenbracket] := ParseRule;
end;


procedure TCompiler.CreateRulesForOpenBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkOpenBrace] := ParseRule;
end;

procedure TCompiler.CreateRulesForCloseBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkCloseBrace] := ParseRule;
end;

procedure TCompiler.CreateRulesForClose_Bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FParseRules[tkCloseBracket] := ParseRule;
end;

procedure TCompiler.CreateRulesForBang;
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

procedure TCompiler.CreateRulesForComma;
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

procedure TCompiler.CreateRulesForComment;
var
  parseRule : pParseRule;
begin
  new(parseRule);

  ParseRule .Prefix := literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FParseRules[tkComment] := ParseRule;
end;

procedure TCompiler.CreateRulesForDivide;
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

procedure TCompiler.CreateRulesForNil;
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

procedure TCompiler.CreateRulesForNotEqual;
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

procedure TCompiler.CreateRulesForNumber;
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


procedure TCompiler.CreateRulesForEqualEqual;
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


procedure TCompiler.CreateRulesForFalse;
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

procedure TCompiler.CreateRulesForTrue;
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

procedure TCompiler.CreateRulesForWhile;
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

procedure TCompiler.CreateRulesForGreaterThan;
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

procedure TCompiler.CreateRulesForGreaterThanEqual;
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

procedure TCompiler.CreateRulesForIdentifier;
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

procedure TCompiler.CreateRulesForIF;
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

procedure TCompiler.CreateRulesForLessThan;
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

procedure TCompiler.CreateRulesForLessThanEqual;
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

procedure TCompiler.CreateRulesForPlus;
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


procedure TCompiler.CreateRulesForPrint;
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


procedure TCompiler.CreateRulesForSemiColon;
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

procedure TCompiler.CreateRulesForString;
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

procedure TCompiler.CreateRulesForMinus;
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

procedure TCompiler.CreateRulesForMultiply;
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

procedure TCompiler.CreateRulesForElse;
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

procedure TCompiler.CreateRulesForEOF;
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

procedure TCompiler.CreateRulesForOR;
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

procedure TCompiler.CreateRulesForAnd;
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




procedure TCompiler.CreateRules ;
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
  CreateRulesForIF;
  CreateRulesForElse;
  CreateRulesForAnd;
  CreateRulesForOr;
  CreateRulesForComma;
  CreateRulesForWhile;
  CreateRulesForEOF;

end;



constructor TCompiler.Create(
  const Constants : TValueList;
  const Globals : TValueList;
  const Tokens  : pTokenIterator;
  const Scanner : TScanner;
  const FunctionKind : TFunctionKind);

begin
  createRules;
  FConstants := Constants;
  FGlobals := Globals;

  Assert(Scanner.TokenCount > 1, 'No text to compile');   //it should have at least 1. (regardless of text scanned, as it always adds 1 extra EOF_TOKEN)

  FScanner := Scanner;
  //  FTokens.Init(Scanner.Tokens);
  FTokens := Tokens;

  FScopeDepth := 0;

  new(FLocals);
  FLocals.Init(true);
  FLocals.Add; //add an empty local for later use internally.

  FFunctionKind := FunctionKind;

  if not(FFunctionKind = TYPE_SCRIPT) then
  begin
    FFunc := newLoxFunction(FTokens.Previous.txt);
    exit;
  end;

  FFunc := newLoxFunction('toplevelcompiler');
end;



destructor TCompiler.destroy;
begin
  disposeFunction(FFunc);
  FLocals.Finalize;
  FConstants.Free;
  inherited;
end;


end.

