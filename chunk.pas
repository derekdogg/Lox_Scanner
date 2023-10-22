unit Chunk;

interface

uses
  Loxtypes,
  ValueList,
  Values,
  IntegerArray,
  ByteArray;


const
  MAX_JUMP =  65535;

type


  TChunks = record
  private
    FConstantCount : integer;
    FBytes       : TDynamicBytes;
    FConstants   : TValueList;
    // FLine      : TIntegers;
  public
    function Constants : TValueList;
    function OpCodes : TDynamicBytes;
    Function EmitBytes(const Operand : TOpCodes; const  byte2 : byte) : integer;overload;
    function EmitBytes(const byte1, byte2 : byte) : Integer;overload;
    function EmitByte(const value : byte) : integer; overload;
    function EmitByte(const Operand : TOpCodes) : integer;overload;
    function MakeConstant(const value : pValue) : integer;
    function EmitConstant(const Value: pValue) : Integer; //rename to check all instances
    function AddReturn : integer;
    Function AddNIL : Integer;
    Function AddTRUE : Integer;
    Function AddFALSE : Integer;
    Function AddPOP : Integer;
   // Function AddGET_LOCAL : Integer;
    Function AddSET_LOCAL : Integer;
    Function AddGET_GLOBAL(const index  : Integer) : integer;
    function AddDEFINE_GLOBAL(const index : byte): Integer;
    Function AddSET_GLOBAL(const index : integer):  Integer;
    Function AddGET_UPVALUE : Integer;
    Function AddSET_UPVALUE : Integer;
    Function AddGET_PROPERTY : Integer;
    Function AddSET_PROPERTY : Integer;
    Function AddGET_SUPER : Integer;
    Function AddEQUAL : Integer;
    function AddNotEQUAL: Integer;
    Function AddGREATER : Integer;
    Function AddLESS : Integer;
    Function AddLESSTHANEQUAL : integer;
    function AddGREATERTHANEQUAL: Integer;
    Function AddADD : Integer;
    Function AddSUBTRACT : Integer;
    Function AddMULTIPLY : Integer;
    Function AddDIVIDE : Integer;
    Function AddNOT : Integer;
    Function AddNEGATE : Integer;
    Function AddPRINT : Integer;
    Function AddJUMP : Integer;
    Function AddJUMP_IF_FALSE : Integer;
    Function AddLOOP : Integer;
    //Function AddCALL : Integer;
    Function AddINVOKE : Integer;
    Function AddSUPER_INVOKE : Integer;
//    Function AddCLOSURE : Integer;
//    Function AddCLOSE_UPVALUE : Integer;
//    Function AddCLASS : Integer;
    Function AddINHERIT : Integer;
    Function AddMETHOD  : Integer;
    procedure init;
    procedure finalize;
  end;
 





implementation
uses
  Exceptions;


function NewOpCode(const OPCode : TOpCodes) : pByte;
begin
  new(result);
  result^ := byte(opCode);
end;

function newByte(const value : byte) : pByte;
begin
  new(result);
  result^ := value;
end;

 function TChunks.EmitByte(const Operand : TOpCodes) : integer;
 begin
   EmitByte(Byte(Operand));
 end;

function TChunks.EmitByte(const value : byte) : integer;
begin
  //writeChunk(currentChunk(), byte, parser.previous.line);
  result := FBytes.Add(newByte(value));
end;


Function TChunks.EmitBytes(const Operand : TOpCodes; const  byte2 : byte) : integer;
begin
  result := emitByte(byte(Operand));
  emitByte(byte2);
end;

Function TChunks.EmitBytes(const byte1, byte2 : byte) : integer;
begin
  result := emitByte(byte1);
  emitByte(byte2);
end;


function TChunks.MakeConstant(const value : pValue) : integer;
begin
  result := FConstants.Add(Value);
  inc(FConstantCount);
end;

function TChunks.OpCodes: TDynamicBytes;
begin
  result := FBytes;
end;

//add constant opcode followed by index of constant in constants array
function TChunks.EmitConstant(const Value: pValue) : Integer;
begin
  assert(Assigned(Value), 'Value for emission is nil.. please try again later. Have a nice day you are screwed');
  if FConstantCount = high(Byte) then raise EMaxConstants.create('Max constants reached'); //note since the opcodes is bytes array, it has fixed index size of 256
  inc(FConstantCount);
  result := EmitBytes(OP_CONSTANT,MakeConstant(Value));
end;

function TChunks.AddDEFINE_GLOBAL(const index : byte) : Integer; //index of the global constant
begin
  assert(index < FConstantCount, 'index for get global is out of range');
  result := EmitBytes(OP_DEFINE_GLOBAL,index);
end;

function TChunks.AddGET_GLOBAL(const Index : integer) : Integer;
begin
  assert(index < FConstantCount, 'index for get global is out of range');
  result := EmitBytes(OP_GET_GLOBAL, index);
end;

function TChunks.AddSET_GLOBAL(const Index : integer): Integer;
begin
  assert(index < FConstantCount, 'index for set global is out of range');
  result := EmitBytes(OP_SET_GLOBAL,index);
end;

function TChunks.AddDIVIDE: Integer;
begin
  result := EmitByte(OP_DIVIDE);
end;

function TChunks.AddEQUAL: Integer;
begin
  result := EmitByte(OP_EQUAL);
end;

function TChunks.AddNotEQUAL: Integer;
begin
  result := AddEqual;
  AddNOT;
end;

function TChunks.AddFALSE: Integer;
begin
  result := EmitByte(OP_FALSE);
end;



(*function TChunks.AddGET_LOCAL: Integer;
begin
  result := FBytes.Add(ord(OP_GET_LOCAL));
end; *)

function TChunks.AddGET_PROPERTY: Integer;
begin
  result := EmitByte(OP_GET_PROPERTY);
end;

function TChunks.AddGET_SUPER: Integer;
begin
  result := EmitByte(OP_GET_SUPER);
end;

function TChunks.AddGET_UPVALUE: Integer;
begin
  result := EmitByte(OP_GET_UPVALUE);
end;

function TChunks.AddGREATER: Integer;
begin
  result := EmitByte(OP_GREATER);
end;

function TChunks.AddGREATERTHANEQUAL: Integer;
begin
  result := AddLESS;
  result := AddNOT;
end;

function TChunks.AddINHERIT: Integer;
begin
  result := EmitByte(OP_INHERIT);
end;

function TChunks.AddINVOKE: Integer;
begin
  result := EmitByte( OP_INVOKE);
end;

function TChunks.AddJUMP: Integer;
begin
 result := EmitByte(OP_JUMP);
end;

function TChunks.AddJUMP_IF_FALSE: Integer;
begin
  result := EmitByte(OP_JUMP_IF_FALSE);
end;

function TChunks.AddLESS: Integer;
begin
  result := EmitByte(OP_LESS);
end;

function TChunks.AddLESSTHANEQUAL: integer;
begin
  result := AddGreater;//FBytes.Add(ord(OP_GREATER));
  result := AddNot;//FBytes.Add(ord(OP_NOT));
end;

function TChunks.AddLOOP: Integer;
begin
  result := EmitByte(OP_LOOP);
end;

function TChunks.AddMETHOD: Integer;
begin
  result := EmitByte(OP_METHOD);
end;

function TChunks.AddMULTIPLY: Integer;
begin
  result := EmitByte(OP_MULTIPLY);
end;

function TChunks.AddNEGATE: Integer;
begin
  result := EmitByte(OP_NEGATE);
end;

function TChunks.AddNIL: Integer;
begin
  result := EmitByte(OP_NIL);
end;

function TChunks.AddNOT: Integer;
begin
  result := EmitByte(OP_NOT);
end;

function TChunks.AddPOP: Integer;
begin
  result := EmitByte(OP_POP);
end;

function TChunks.AddPRINT: Integer;
begin
  result := EmitByte(OP_PRINT);
end;

function TChunks.AddReturn : integer;
begin
  result := EmitByte(OP_RETURN);
end;



function TChunks.AddSET_LOCAL: Integer;
begin
  result := EmitByte(OP_SET_LOCAL);
end;

function TChunks.AddSET_PROPERTY: Integer;
begin
   result := EmitByte(OP_SET_PROPERTY);
end;

function TChunks.AddSET_UPVALUE: Integer;
begin
  result := EmitByte(OP_SET_UPVALUE);
end;


function TChunks.AddADD: Integer;
begin
  result := EmitByte(OP_ADD);
end;

function TChunks.AddSUBTRACT: Integer;
begin
  result := EmitByte(OP_SUBTRACT);
end;

function TChunks.AddSUPER_INVOKE: Integer;
begin
  result := EmitByte(OP_SUPER_INVOKE);
end;

function TChunks.AddTRUE: Integer;
begin
  result := EmitByte(OP_TRUE);
end;

function TChunks.Constants: TValueList;
begin
  result := FConstants;
end;

procedure TChunks.finalize;
begin
  FBytes.Finalize;
  FConstants.Finalize;
end;

procedure TChunks.init;
begin
   FBytes.Init(true);
   FConstants.Init(true); //list ownership of pointers
end;


 

end.
