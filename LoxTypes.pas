unit LoxTypes;

interface


const
  cMaxTokens = 10000;
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



TTokenKind = (
  tkNull,
  tkNumber,
  tkQuotes,
  tkUnterminatedQuotes,
  tkopen_bracket,
  tkClose_Bracket,
  tkAsterisk,
  tkPlus,
  tkComma,
  tkminus,
  tkdot,
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
  tkunderscore);

 const




  TTokenName : Array[tkNull..tkUnderscore] of string = (
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
  'underscore');


type


  TToken = record
    kind: TTokenKind;
    start: integer;
    length: integer;
    line: integer;
    text : string;
  end;

  TTokens = Array[0..cMaxTokens] of TToken;

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




implementation


end.
