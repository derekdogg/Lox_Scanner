unit ObjectFunction;

interface

uses
  Chunk,
  Loxtypes;

type
 
  TFunctionKind = (TYPE_FUNCTION,TYPE_SCRIPT);

  pLoxFunction = ^TLoxFunction;
  TLoxFunction = record
  private
    LoxObject : TLoxObject;
    FuncKind : TFunctionKind;
    Arity : integer;
    Name : String;
    Chunks : TChunks;
  end;

  (*TCallFrame = record
    LoxFunction : pLoxObject;
    uint8_t* ip;
    Value* slots;
  end; *)






  function newLoxFunction : pLoxFunction;

implementation

  function newLoxFunction : pLoxFunction;
  begin
    new(result);
    result.LoxObject.Kind := OBJ_FUNCTION;
    result.LoxObject.Next := nil;
    result.FuncKind := Type_Function;
    result.Arity := 0;
    result.Name := '';
    result.Chunks.Init;
  end;


end.
