unit LoxTypes;

interface
uses
  classes;


const

  MAX_JUMP =  65535;

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




 TFunctionKind = (TYPE_FUNCTION,TYPE_SCRIPT);

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
    PREC_CALL,
    PREC_SUBSCRIPT,
    PREC_PRIMARY        // . ()

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
  tkOpenSquareBracket,
  tkCloseSquareBracket,
  tkopenbracket,
  tkcloseBracket,
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
  tkLessthan,
  tkGreaterThanEqual,
  tkgreaterthan,
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
  tkComment,
  tkSemicolon,
  tkEOF);

 const




  TTokenName : Array[tkNull..tkEOF] of string = (
  'Null',
  'Number',
  'Quotes',
  'UnterminatedQuotes',
  'OpenSquareBracket',
  'CloseSquareBracket',
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
  'Comment',
  'Semicolon',
  'EOF');


type


  TToken = class
    kind: TTokenKind;
    start: word;
    length: word;
    line: word;
    txt : string;
    procedure toStrings(const strings : TStrings);
  end;

//  TTokens = Array[0..cMaxTokens] of TToken;



   




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
    OBJ_UPVALUE,

    OBJ_LIST
    //< Closures obj-type-upvalue
  );

  pLoxObject = ^TLoxObject;
  TLoxObject = record
    Kind     : TObjectKind;
    IsMarked : Boolean;
    Next     : pLoxObject;
  end;



  pLoxString = ^TLoxString;
  TLoxString = record
    Obj    : TLoxObject;
    Chars  : String;
  end;



  //default values to nil;
  TLoxKind = (lxNull,lxNumber,lxBoolean, lxObject, lxString, lxList,lxFunction, lxNative);



  TNumber = Double;



  (*pByteCode = ^TByteCode;
  TByteCode = record
    //Operation : TOPCODES;
    Value     : pValue; //value associated with operation
  end; *)

//  function NewLoxObject : pLoxObject;

//  function LoxObjectFrom(const value : pointer) : pLoxObject;





//  function LoxStringFrom(const pObject : pLoxObject) : pLoxString;
 
  function GetHashString(const value : string) : UInt64;
  //--------------------------------------------------------------

   function TokenKindToStr(const TokenKind : TTokenKind) : string;


 


implementation

uses typinfo, sysUtils;
 


function TokenKindToStr(const TokenKind : TTokenKind) : string;
begin
  result := GetEnumName(typeInfo(TTokenKind ), Ord(TokenKind));
end;


(*function LoxObjectFrom(const value : pointer) : pLoxObject;
begin
   assert(Assigned(value));
   result := @value^;
end; *)

(*function LoxStringFrom(const pObject : pLoxObject) : pLoxString;
begin
  assert(Assigned(pObject));
  assert(pObject.Kind = OBJ_STRING);
  result :=  pLoxString(pObject);
end;

function NewLoxObject : pLoxObject;
begin
  new(Result);
  fillchar(result^,sizeof(TLoxObject),#0);
end;
*)


function GetHashString(const value : string) : UInt64;
  (*
    algorithm fnv-1 is
    hash := FNV_offset_basis

    for each byte_of_data to be hashed do
        hash := hash x FNV_prime
        hash := hash XOR byte_of_data

    return hash
  *)
const
  FNV_offset_basis  =  $811C9DC5;
  FNV_prime         =  $1000193;

var
  i: Integer;

begin
  result := FNV_offset_basis;
  for i := 1 to length(value) do
  begin
    result := ((result XOR ord(value[i]))* FNV_prime) and $FFFFFFFF;
  end;
end;



procedure TToken.toStrings(const strings: TStrings);
const
  ln = 'Kind : %s, Start: %s, Length : %s, Line : %s';
var
  txt : string;
begin
  txt := Format(ln,[TTokenName[Kind], inttostr(Start),inttostr(Length),inttostr(Line)]);
  strings.add(txt);
end;

end.
