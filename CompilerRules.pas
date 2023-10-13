unit CompilerRules;

interface

uses
  compiler;

type

  TCompilerRules = class
  private
    FCompiler : TCompiler;
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





  public

    procedure CreateRules(const Compiler : TCompiler);
  end;





implementation
uses
  LoxTypes;




{ TCompilerRules }

procedure TCompilerRules.CreateRuleForOpen_bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Prefix := FCompiler.grouping;
  ParseRule.Infix := FCompiler.call;
  ParseRule.Precedence := PREC_CALL;
  FCompiler.ParseRule[tkOpenbracket] := ParseRule;
end;


procedure TCompilerRules.CreateRulesForOpenBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkOpenBrace] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForCloseBrace;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkCloseBrace] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForClose_Bracket;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkCloseBracket] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForBang;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE}
  ParseRule.Prefix := FCompiler.unary;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkBang] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForComment;
var
  parseRule : pParseRule;
begin
  new(parseRule);

  ParseRule .Prefix := FCompiler.literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkComment] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForDivide;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_FACTOR;
  FCompiler.ParseRule[tkSlash] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForNil;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},

  ParseRule .Prefix := FCompiler.literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkNil] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForNotEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_EQUALITY;
  FCompiler.ParseRule[tkBangEqual] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForNumber;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_NUMBER]   = {number,   NULL,   PREC_NONE},

  ParseRule .Prefix := FCompiler.number;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkNumber] := ParseRule;
end;


procedure TCompilerRules.CreateRulesForEqualEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_EQUALITY;
  FCompiler.ParseRule[tkEqualEqual] := ParseRule;
end;


procedure TCompilerRules.CreateRulesForFalse;
var
  parseRule : pParseRule;
begin
  new(parseRule);
 //[TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE}

  ParseRule .Prefix := FCompiler.literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkFalse] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForTrue;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE}

  ParseRule .Prefix := FCompiler.literal;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkTrue] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForWhile;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkWhile] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForGreaterThan;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON}

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_COMPARISON;
  FCompiler.ParseRule[tkGreaterthan] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForGreaterThanEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},


  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_COMPARISON;
  FCompiler.ParseRule[tkGreaterthanequal] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForIdentifier;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//   [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE}

  ParseRule.Prefix := FCompiler.variable;
  ParseRule.Infix := nil;
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkIdentifier] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForIF;
var
  parseRule : pParseRule;
begin
  new(parseRule);
//  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},

  ParseRule.Prefix := nil;
  ParseRule.Infix := FCompiler.binary;
  ParseRule.Precedence := PREC_NONE;
  FCompiler.ParseRule[tkIf] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForLessThan;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  // [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},

  ParseRule.Prefix := nil;
  ParseRule.Infix := FCompiler.binary;
  ParseRule.Precedence := PREC_COMPARISON;
  FCompiler.ParseRule[tkLessThan] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForLessThanEqual;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},

  ParseRule.Prefix := nil;
  ParseRule.Infix := FCompiler.binary;
  ParseRule.Precedence := PREC_COMPARISON;
  FCompiler.ParseRule[tkLessThanEqual] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForPlus;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},


  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_TERM;
  FCompiler.ParseRule[tkPlus] := ParseRule;
end;


procedure TCompilerRules.CreateRulesForPrint;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE}

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkPrint] := ParseRule;
end;


procedure TCompilerRules.CreateRulesForSemiColon;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE}

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkSemiColon] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForString;
var
  parseRule : pParseRule;
begin
  new(parseRule);
   //[TOKEN_STRING]        = {string,   NULL,   PREC_NONE},

  ParseRule .Prefix := FCompiler.strings;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkQuotes] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForMinus;
var
  parseRule : pParseRule;
begin
  new(parseRule);
 // [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},


  ParseRule .Prefix := FCompiler.unary;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_TERM;
  FCompiler.ParseRule[tkMinus] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForMultiply;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.binary;
  ParseRule .Precedence := PREC_FACTOR;
  FCompiler.ParseRule[tkAsterisk] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForElse;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},

  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkElse] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForEOF;
var
  parseRule : pParseRule;
begin
  new(parseRule);
// [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},


  ParseRule .Prefix := nil;
  ParseRule .Infix := nil;
  ParseRule .Precedence := PREC_NONE;
  FCompiler.ParseRule[tkEof] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForOR;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_OR]            = {NULL,     or_,    PREC_OR},

  ParseRule .Prefix := nil;
  ParseRule .Infix := FCompiler.Or_;
  ParseRule .Precedence := PREC_OR;
  FCompiler.ParseRule[tkOr] := ParseRule;
end;

procedure TCompilerRules.CreateRulesForAnd;
var
  parseRule : pParseRule;
begin
  new(parseRule);
  //[TOKEN_AND]           = {NULL,     and_,   PREC_AND},

  ParseRule .Prefix := nil;
  ParseRule.Infix := FCompiler.and_;
  ParseRule.Precedence := PREC_AND;
  FCompiler.ParseRule[tkAnd] := ParseRule;
end;




procedure TCompilerRules.CreateRules(const Compiler: TCompiler);
begin
  FCompiler := Compiler;
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
  CreateRulesForWhile;
  CreateRulesForEOF;

end;


end.
