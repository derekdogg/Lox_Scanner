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
  tkComment,
  tkSemicolon,
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
  'Comment',
  'Semicolon',
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

    procedure Init;
  end;

  // Laid out in memory like
  // [TLoxObject][Length][chars]
  // so if you get a pointer (pLoxObject) to TLoxString address then increment it you will sit on the chars in memory)

  //from book pp561.
(*Given an ObjString*, you can safely cast it to Obj* and then access the
type field from it. *)

  pLoxString = ^TLoxString;
  TLoxString = record
  private
    FObj    : TLoxObject;
    Flength : integer;
    Fchars  : String;
    Fhash   : UInt64;
    function getChars: string;
    function getHash: UInt64;
    procedure hashString;
    procedure setChars(const Value: String);
  public
    Property Obj    : TLoxObject read FObj;
    Property Chars  : string read getChars write setChars;
    Property Hash   : Uint64 read getHash;
    procedure Init;
  end;





  TLoxKind = (lxNumber,lxBoolean, lxNull, lxObject);
  TNumber = Double;

  pValue = ^TValue;
  TEightBytes = array[0..7] of byte;
  TValue = record
  private
    
    FKind  : TLoxKind;
    FValue : TEightBytes;
    function getNumber  : TNumber;
    procedure SetNumber(const value : TNumber);

    function getBoolean : Boolean;
    procedure setBoolean(const value : Boolean);

    procedure setObject(const value : pLoxObject);
    function  getObject : pLoxObject;

    procedure setNull(const value : Boolean);
    function getNull: boolean;
    procedure SetString(Const value : String);
    function GetString : String;
    function getIsNumber: Boolean;
    function getIsStringObject: Boolean;
    function getIsObject : Boolean;

  public
    property IsObject       : Boolean read getisobject;
    property IsStringObject : Boolean read getIsStringObject;
    property IsNumber       : Boolean read getIsNumber;
    property Kind           : TLoxKind read FKind write FKind;
    property Number         : TNumber read getNumber write SetNumber;
    property Boolean        : Boolean read getBoolean write setBoolean;
    property Str            : String read getString write setString;
    property Bytes          : TEightBytes read FValue;
    property LoxObject      : pLoxObject read getObject write setObject;
    property ToString       : string read getString;
    property Null           : boolean read getNull write setNull;
  end;


  (*pByteCode = ^TByteCode;
  TByteCode = record
    //Operation : TOPCODES;
    Value     : pValue; //value associated with operation
  end; *)

  (*function NewLoxObject : pLoxObject;
  function NewLoxString(Const str : string) : pLoxString;
  function LoxObjectFrom(const pString : pLoxString) : pLoxObject; //going up the hierarchy
  function LoxStringFrom(const pObject : pLoxObject) : pLoxString;
  function StringValue(const str : string) : TValue; *)
  function GetHashString(const value : string) : UInt64;
  //--------------------------------------------------------------

  function NewString(const str : String) : pValue;
  function NewNumber(Const number : TNumber) : pValue;
  function NewBool(const Bool : Boolean) : pValue;
  function DisposeValue(value : pValue) : boolean;

implementation

uses sysUtils;

function LoxObjectFrom(const pString : pLoxString) : pLoxObject;
begin
   assert(Assigned(pString));
   result := @pString^;
end;

function NewLoxString(Const Str : String) : pLoxString;
begin
  new(Result);
  result.init;
  result.Chars := Copy(str,1,length(str));
end;

function StringValue(const str : string) : TValue;
begin
  result.LoxObject := LoxObjectFrom(NewLoxString(str));
end;



function LoxStringFrom(const pObject : pLoxObject) : pLoxString;
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

function NewString(const str : String) : pValue;
begin
  new(result);
  result.LoxObject := LoxObjectFrom(NewLoxString(str));
end;

function NewBool(const Bool : Boolean) : pValue;
begin
  new(result);
  result.Boolean := Bool;
end;

function NewNumber(const number : TNumber) : pValue;
begin
  new(result);
  result.Number := Number;
end;

function DisposeValue(value : pValue) : boolean;
var
  loxString : pLoxString;
begin
  assert(assigned(Value), ' value for disposal is nil');
  if value.IsStringObject then
  begin
    assert(value.IsStringObject = true, 'expected a lox object on disposal');
    assert(assigned(Value.LoxObject), 'Lox Object for disposal is not assigned');
    LoxString := pLoxString(Value.LoxObject);
    dispose(LoxString);
  end;
  dispose(Value);
  value := nil;
end;




(*
function StrPCopy(const Source: AnsiString; Dest: PAnsiChar): PAnsiChar;
begin
  Move(PAnsiChar(Source)^, Dest^, Length(Source) + 1); // +1 for the 0 char
  Result := Dest;
end;  *)



procedure TLoxObject.Init;
begin
  fillchar(Self,sizeof(Self),#0);
end;

function TLoxString.getChars: String;
begin
  result := FChars;
end;

function TLoxString.getHash: UInt64;
begin
  result := FHash;
end;

procedure TLoxString.setChars(const Value: String);
begin
  FChars := Value;
  FLength := Length(Value);
  hashString;
end;


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
    result := ((result XOR ord(value[i]))* FNV_prime);// and $FFFFFFFF;
  end;
end;


procedure TLoxString.hashString;
begin
  FHash :=  getHashString(FChars)
end;


procedure TLoxString.Init;
begin
  fillchar(Self,sizeof(Self),#0);
  FObj.Kind := OBJ_STRING;
end;



procedure TValue.setNull(const value : Boolean);
begin
  FKind := lxNull;
  fillchar(FValue,sizeof(FValue),#0);
end;

procedure TValue.setObject(const value : pLoxObject);
begin
  FKind := lxObject;
  Move(Longint(value),FValue[0], SizeOf(Value));
end;

procedure TValue.SetString(const value: String);
var
  Obj : pLoxObject;
begin
  Assert(getIsStringObject = true, 'Value is not a string object');
  Obj := GetObject;
  if Obj <> nil then
    pLoxString(Obj).Chars := Value;
end;

function TValue.getObject : pLoxObject;
begin
  result := nil;
  if FKind = lxObject then
    Move(FValue[0], Result, SizeOf(Result));
end;

function TValue.GetString: String;
var
  Obj : pLoxObject;
begin
  case FKind of
   lxObject : begin
       Obj := GetObject;
       case Obj.Kind of
         OBJ_STRING : begin
           result := pLoxString(obj).Chars;
         end;
       end;
   end;


   lxBoolean : begin
     result := BoolToStr(getBoolean,true);
   end;

   lxNumber : begin
     result := floatToStr(GetNumber);
   end;

   lxNull  : begin
     result := 'null';
   end;
  end;
end;

function TValue.getBoolean : Boolean;
begin
  result := false;
  if FKind = lxObject then
  begin
    result := getNumber <=0;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(Result))
end;

procedure TValue.setBoolean(const value : Boolean);
begin
  assert(FKind <> lxObject,'value is an object, and you are trying to set it to false'); //this is not cool to set the bytes of an existing pointer?
  FKind := lxBoolean;
  FillChar(FValue,Sizeof(FValue),#0);
  Move(value, FValue[0], SizeOf(Value))
end;

function TValue.getIsNumber: Boolean;
begin
  result := FKind = lxNumber;
end;

function TValue.getIsObject: Boolean;
begin
   result := (fKind = lxObject)
end;

function TValue.getIsStringObject: Boolean;
begin
  result := getisobject and (getObject.Kind = OBJ_STRING);
end;

Function TValue.getNull: boolean;
begin
  result := fKind = lxNull;
end;

function TValue.getNumber : TNumber;
var
  l : Longint;
begin
  if FKind = lxObject then
  begin
    Move(FValue[0], l, SizeOf(l));
    result := l;
    exit;
  end;
  Move(FValue[0], Result, SizeOf(Result))
end;

procedure TValue.SetNumber(const value : TNumber);
begin
   Fkind := lxNumber;
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


{ TLoxObject }




end.
