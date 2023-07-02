unit LoxTypes;

interface


const
//  cMaxTokens = 10000;
  cMaxLines = 1000;
  cCR = #13;     //- Carriage return (ASCII)
  cLF = #10;     //- Line feed (ASCII)
  cCRLF = cCR + cLF; //2 chars
  cCRHex =#$D;   //- Carriage return (Hexadecimal)  Note : direct access into string with index will convert to ascii
  cLFHex =#$A;   //- Line feed (Hexadecimal)
  cNULL = #0;     //- NULL
  cTab = #9;      //- Tab
  cSpace = #32;   //- Space


type

  TPrecedence =(
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



  TAscii = (
    Null = 0,
    Space	=	32,
    Bang	=	33,  //!
    quotes	=	34,
    Hash	=	35,   // number sign #
    Dollar	=	36,
    Percent	=	37,
    Ampersand	=	38,
    Single_quote	=	39,
    Open_Bracket	=	40,
    Close_bracket	=	41,
    Asterisk	=	42,
    Plus	=	43,
    Comma	=	44,
    minus	=	45,  // -
    dot	=	46,
    Slash	=	47,
    Zero	=	48,
    One	=	49,
    Two	=	50,
    Three	=	51,
    Four	=	52,
    Five	=	53,
    Six	=	54,
    Seven	=	55,
    Eight	=	56,
    Nine	=	57,
    Colon	=	58,
    Semicolon	=	59,
    Less_than =	60,
    Equal	=	61,
    Greater_than 	=	62,
    Question_mark	=	63,
    At_sign	=	64,
    Uppercase_A	=	65,
    Uppercase_B	=	66,
    Uppercase_C	=	67,
    Uppercase_D	=	68,
    Uppercase_E	=	69,
    Uppercase_F	=	70,
    Uppercase_G	=	71,
    Uppercase_H	=	72,
    Uppercase_I	=	73,
    Uppercase_J	=	74,
    Uppercase_K	=	75,
    Uppercase_L	=	76,
    Uppercase_M	=	77,
    Uppercase_N	=	78,
    Uppercase_O	=	79,
    Uppercase_P	=	80,
    Uppercase_Q	=	81,
    Uppercase_R	=	82,
    Uppercase_S	=	83,
    Uppercase_T	=	84,
Uppercase_U	=	85,
Uppercase_V	=	86,
Uppercase_W	=	87,
Uppercase_X	=	88,
Uppercase_Y	=	89,
Uppercase_Z	=	90,
Open_Square_Bracket	=	91,
Backslash	=	92,
Close_Square_Bracket	=	93,
Caret	=	94,
Underscore	=	95,
Grave_Accent	=	96,
Lowercase_a	=	97,
Lowercase_b	=	98,
Lowercase_c	=	99,
Lowercase_d	=	100,
Lowercase_e	=	101,
Lowercase_f	=	102,
Lowercase_g	=	103,
Lowercase_h	=	104,
Lowercase_i	=	105,
Lowercase_j	=	106,
Lowercase_k	=	107,
Lowercase_l	=	108,
Lowercase_m	=	109,
Lowercase_n	=	110,
Lowercase_o	=	111,
Lowercase_p	=	112,
Lowercase_q	=	113,
Lowercase_r	=	114,
Lowercase_s	=	115,
Lowercase_t	=	116,
Lowercase_u	=	117,
Lowercase_v	=	118,
Lowercase_w	=	119,
Lowercase_x	=	120,
Lowercase_y	=	121,
Lowercase_z	=	122,
Curly_Opening_brace	=	123,
Vertical_bar	=	124,
Curly_Closing_brace	=	125,
tilde	=	126,
Delete	=	127);

(*
typedef enum {
  // Single-character tokens.
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

  TOKEN_ERROR, TOKEN_EOF
} TokenType; *)

TTokenKind = (
  tkNull,
  tkNumber,
  tkQuotes,
  tkUnterminatedQuotes,
  tkopen_bracket,
  tkclose_Bracket,
  tkAsterisk,
  tkPlus,
  tkComma,
  tkminus,
  tkdot,
  tkSlash,
  tkBangEqual,
  tkBang,
  tkEqual,
  tkEqualEqual,
  tkLessThanEqual,
  tkLess_than,
  tkGreaterThanEqual,
  tkgreater_than,
  tkAnd,
  tkClass,
  tkElse,
  tkFalse,
  tkFun,
  tkFor,
  tkIf,
  tkNil,
  tkOr,
  tkPrint,
  tkReturn,
  tkTrue,
  tkVar,
  tkWhile,
  tkIdentifier,
  tkWhiteSpace,
  tkOpenBrace,
  tkCloseBrace,
  tkInteger,
  tkunderscore,
  tkThis,
  tkEOF);

 const




  TTokenName : Array[tkNull..tkEOF] of string = (
  'Null',
  'Number',
  'Quotes',
  'UnterminatedQuotes',
  'open_bracket',
  'Close_Bracket',
  'Asterisk',
  'Plus',
  'Comma',
  'minus',
  'dot',
  'Slash',
  'BangEqual',
  'Bang',
  'Equal',
  'EqualEqual',
  'LessThanEqual',
  'Less_than',
  'GreaterThanEqual',
  'greater_than',
  'And',
  'Class',
  'Else',
  'False',
  'Fun',
  'For',
  'If',
  'Nil',
  'Or',
  'Print',
  'Return',
  'True',
  'Var',
  'While',
  'Identifier',
  'WhiteSpace',
  'CurlyOpenBrace',
  'CurlyCloseBrace',
  'Integer',
  'underscore',
  'this',
  'EOF');


type

  pToken = ^TToken;
  TToken = record
    kind: TTokenKind;
    start: word;
    length: word;
    line: word;
  end;

//  TTokens = Array[0..cMaxTokens] of TToken;



  TOpCodes = (
        OP_NULL,
        OP_CONSTANT,
        OP_NIL,
        OP_TRUE,
        OP_FALSE,
        OP_POP,
        OP_GET_LOCAL,
        OP_SET_LOCAL,
        OP_GET_GLOBAL,
        OP_DEFINE_GLOBAL,
        OP_SET_GLOBAL,
        OP_GET_UPVALUE,
        OP_SET_UPVALUE,
        OP_GET_PROPERTY,
        OP_SET_PROPERTY,
        OP_GET_SUPER,
        OP_EQUAL,
        OP_GREATER,
        OP_LESS,
        OP_ADD,
        OP_SUBTRACT,
        OP_MULTIPLY,
        OP_DIVIDE,
        OP_NOT,
        OP_NEGATE,
        OP_PRINT,
        OP_JUMP,
        OP_JUMP_IF_FALSE,
        OP_LOOP,
        OP_CALL,
        OP_INVOKE,
        OP_SUPER_INVOKE,
        OP_CLOSURE,
        OP_CLOSE_UPVALUE,
        OP_RETURN,
        OP_CLASS,
        OP_INHERIT,
        OP_METHOD
);




const

  TOP_Code_name : Array[OP_NULL..OP_METHOD] of string  = (
   'OP_NULL',
//> op-constant
  'OP_CONSTANT',
//< op-constant
//> Types of Values literal-ops
  'OP_NIL',
  'OP_TRUE',
  'OP_FALSE',
//< Types of Values literal-ops
//> Global Variables pop-op
  'OP_POP',
//< Global Variables pop-op
//> Local Variables get-local-op
  'OP_GET_LOCAL',
//< Local Variables get-local-op
//> Local Variables set-local-op
  'OP_SET_LOCAL',
//< Local Variables set-local-op
//> Global Variables get-global-op
  'OP_GET_GLOBAL',
//< Global Variables get-global-op
//> Global Variables define-global-op
  'OP_DEFINE_GLOBAL',
//< Global Variables define-global-op
//> Global Variables set-global-op
  'OP_SET_GLOBAL',
//< Global Variables set-global-op
//> Closures upvalue-ops
  'OP_GET_UPVALUE',
  'OP_SET_UPVALUE',
//< Closures upvalue-ops
//> Classes and Instances property-ops
  'OP_GET_PROPERTY',
  'OP_SET_PROPERTY',
//< Classes and Instances property-ops
//> Superclasses get-super-op
  'OP_GET_SUPER',
//< Superclasses get-super-op
//> Types of Values comparison-ops
  'OP_EQUAL',
  'OP_GREATER',
  'OP_LESS',
//< Types of Values comparison-ops
//> A Virtual Machine binary-ops
  'OP_ADD',
  'OP_SUBTRACT',
  'OP_MULTIPLY',
  'OP_DIVIDE',
//> Types of Values not-op
  'OP_NOT',
//< Types of Values not-op
//< A Virtual Machine binary-ops
//> A Virtual Machine negate-op
  'OP_NEGATE',
//< A Virtual Machine negate-op
//> Global Variables op-print
  'OP_PRINT',
//< Global Variables op-print
//> Jumping Back and Forth jump-op
  'OP_JUMP',
//< Jumping Back and Forth jump-op
//> Jumping Back and Forth jump-if-false-op
  'OP_JUMP_IF_FALSE',
//< Jumping Back and Forth jump-if-false-op
//> Jumping Back and Forth loop-op
  'OP_LOOP',
//< Jumping Back and Forth loop-op
//> Calls and Functions op-call
  'OP_CALL',
//< Calls and Functions op-call
//> Methods and Initializers invoke-op
  'OP_INVOKE',
//< Methods and Initializers invoke-op
//> Superclasses super-invoke-op
  'OP_SUPER_INVOKE',
//< Superclasses super-invoke-op
//> Closures closure-op
  'OP_CLOSURE',
//< Closures closure-op
//> Closures close-upvalue-op
  'OP_CLOSE_UPVALUE',
//< Closures close-upvalue-op
  'OP_RETURN',
//> Classes and Instances class-op
  'OP_CLASS',
//< Classes and Instances class-op
//> Superclasses inherit-op
  'OP_INHERIT',
//< Superclasses inherit-op
//> Methods and Initializers method-op
  'OP_METHOD'
//< Methods and Initializers method-op;
);

Type
  TObjectKind = (
    //> Methods and Initializers obj-type-bound-method
    OBJ_BOUND_METHOD,
    //< Methods and Initializers obj-type-bound-method
    //> Classes and Instances obj-type-class
    OBJ_CLASS,
    //< Classes and Instances obj-type-class
    //> Closures obj-type-closure
    OBJ_CLOSURE,
    //< Closures obj-type-closure
    //> Calls and Functions obj-type-function
    OBJ_FUNCTION,
    //< Calls and Functions obj-type-function
    //> Classes and Instances obj-type-instance
    OBJ_INSTANCE,
    //< Classes and Instances obj-type-instance
    //> Calls and Functions obj-type-native
    OBJ_NATIVE,
    //< Calls and Functions obj-type-native
    OBJ_STRING,
    //> Closures obj-type-upvalue
    OBJ_UPVALUE
    //< Closures obj-type-upvalue
  );

  pLoxObject = ^TLoxObject;
  TLoxObject = record
    Kind     : TObjectKind;
    IsMarked : Boolean;
    Next     : pLoxObject;
  end;

  TKind = (tvNumber,tvBoolean, tvNull, tvObject);
  TNumber = Double;

  pValue = ^TValue;
  TEightBytes = array[0..7] of byte;
  TValue = record
  private
    FKind  : TKind;
    FValue : TEightBytes;
    function getNumber  : TNumber;
    procedure SetNumber(const value : TNumber);

    function getBoolean : Boolean;
    procedure setBoolean(const value : Boolean);

    procedure setObject(const value : pLoxObject);
    function  getObject : pLoxObject;

    procedure setNull(const value : Boolean);
    function getNull: boolean;

    function GetString : String;
  public
   property Kind       : TKind read FKind write FKind;
    property Number     : TNumber read getNumber write SetNumber;
    property Boolean    : Boolean read getBoolean write setBoolean;
    property Bytes      : TEightBytes read FValue;
    property LoxObject  : pLoxObject read getObject write setObject;
    property ToString   : string read getString;
    property Null       : boolean read getNull write setNull;
  end;


  pByteCode = ^TByteCode;
  TByteCode = record
    Operation : TOPCODES;
    Value     : TValue; //value associated with operation
  end;




implementation
uses sysUtils;

procedure TValue.setNull(const value : Boolean);
begin
  FKind := tvNull;
  fillchar(FValue,sizeof(FValue),#0);
end;

procedure TValue.setObject(const value : pLoxObject);
begin
  FKind := tvObject;
  Move(Longint(value),FValue[0], SizeOf(Value));
end;

function TValue.getObject : pLoxObject;
begin
  result := nil;
  if FKind = tvObject then
    Move(FValue[0], Result, SizeOf(Result));
end;

function TValue.GetString: String;
begin
  case FKind of
   tvObject : begin
     result := '';
   end;
   tvBoolean : begin
     result := BoolToStr(getBoolean,true);
   end;

   tvNumber : begin
     result := floatToStr(GetNumber);
   end;
   end;
end;

function TValue.getBoolean : Boolean;
begin
  result := false;
  if FKind = tvObject then
  begin
    result := getNumber <=0;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(Result))
end;

procedure TValue.setBoolean(const value : Boolean);
begin
  assert(FKind <> tvObject); //this is not cool to set the bytes of an existing pointer?
  FKind := tvBoolean;
  FillChar(FValue,Sizeof(FValue),#0);
  Move(value, FValue[0], SizeOf(Value))

end;

function TValue.getNull: boolean;
begin
  result := fKind = tvNull;
end;

function TValue.getNumber : TNumber;
var
  l : Longint;
begin
  if FKind = tvObject then
  begin
    Move(FValue[0], l, SizeOf(l));
    result := l;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(Result))
end;

procedure TValue.SetNumber(const value : TNumber);
begin
   Fkind := tvNumber;
   Move(value,FValue[0], SizeOf(Value))
end;


(*

procedure parsePrecedence(precedence: TPrecedence);
begin

  advance();
  var prefixRule: ParseFn := getRule(parser.previous.typ).prefix;
  if not Assigned(prefixRule) then
  begin
    error('Expect expression.');
    Exit;
  end;
  //< precedence-body
end;
procedure expression();
begin
  parsePrecedence(PREC_ASSIGNMENT);
end;

procedure grouping(canAssign : boolean);
begin
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
end;


type
  ParseRule = record
    prefix: Pointer;
    infix: Pointer;
    precedence: Integer;
  end;

const
  rules: array[tkopen_bracket..tkclose_Bracket] of ParseRule = (
    (prefix: @grouping; infix: @call; precedence: PREC_CALL),
    (prefix: nil; infix: nil; precedence: PREC_NONE)
  );

*)

{ TToken }


end.
