unit ObjectFunction;

interface

uses
  Chunk,
  Loxtypes;

type
 
   


  (* When the compiler reaches a function declaration, it needs to emit code into the
     function’s chunk when compiling its body.

      At the end of the function body, the compiler needs to return to the
      previous chunk it was working with

     *)

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
  private
    LoxObject : pLoxObject;
    FuncKind : TFunctionKind;
    Arity : integer; // The arity field stores the number of parameters the function expects.
    Name : String;
    Chunks : TChunks;
  end;

(*
typedef struct {
  Obj obj;
  int arity;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

  (*TCallFrame = record
    LoxFunction : pLoxObject;
    uint8_t* ip;
    Value* slots;
  end; *)


  function newLoxFunction(const Name : String) : pLoxFunction;

implementation

  function newLoxFunction(const Name : String) : pLoxFunction;
  begin
    new(result);
    result.LoxObject := NewLoxObject;
    result.LoxObject.Kind := OBJ_FUNCTION;
    result.LoxObject.Next := nil;
    result.FuncKind := Type_Function;
    result.Arity := 0;
    result.Name := Name;
    result.Chunks.Init;
  end;


end.
