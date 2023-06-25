unit Chunk;

interface

uses
  Loxtypes,
  DoubleArray,
  IntegerArray,
  ByteArray;

type

  TConstantChunk = record
    OpCode    : TOpCodes;
    constant  : Double;
  end;

  TChunks = record
  private
    FConstantCount : integer;
    FOpCodes       : TBytes;
    FConstants     : TDoubles;
    // FLine      : TIntegers;
  public
    function Constants : TDoubles;
    function AddConstant(const Value: Double) : Integer;
    function AddReturn : integer;
    Function AddNIL : Integer;
    Function AddTRUE : Integer;
    Function AddFALSE : Integer;
    Function AddPOP : Integer;
    Function AddGET_LOCAL : Integer;
    Function AddSET_LOCAL : Integer;
    Function AddGET_GLOBAL : Integer;
    Function AddDEFINE_GLOBAL : Integer;
    Function AddSET_GLOBAL : Integer;
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
    Function AddCALL : Integer;
    Function AddINVOKE : Integer;
    Function AddSUPER_INVOKE : Integer;
    Function AddCLOSURE : Integer;
    Function AddCLOSE_UPVALUE : Integer;
    Function AddCLASS : Integer;
    Function AddINHERIT : Integer;
    Function AddMETHOD  : Integer;
    procedure init;
    procedure finalize;
  end;
 
  //use for dissasembly? Maybe useful
  TInstructionPointer = record
  private
    FBytes      : TBytes;
    FConstants  : TDoubles;
    FIndex      : integer;
    FCurrent    : pByte;
    FPrevious   : pByte;
  public
    function ByteCount : integer;
    function ConstantCount : integer;
    function Constant(const Index : byte) : pDouble;
    function ByteAt(const Index : integer) : pByte;
    function Index : integer;
    function Current : pByte;
    function Next : pByte;
    function PeekNext : pByte;
    procedure Init(const Chunks: TChunks);
  end;





implementation
uses
  Exceptions;


//add the opcode constant to the opcodes, add a constant to the constants, then add the index of the contant to the opcodes.
function TChunks.AddADD: Integer;
begin
  result := FOPCodes.Add(ord(OP_ADD));
end;

function TChunks.AddCALL: Integer;
begin
  result := FOPCodes.Add(ord(OP_CALL));
end;

function TChunks.AddCLASS: Integer;
begin
  result := FOPCodes.Add(ord(OP_CLASS));
end;

function TChunks.AddCLOSE_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_CLOSE_UPVALUE));
end;

function TChunks.AddCLOSURE: Integer;
begin
  result := FOPCodes.Add(ord(OP_CLOSURE));
end;

//add constant opcode followed by index of constant in constants array
function TChunks.AddConstant(const Value: Double) : Integer;
begin
  if FConstantCount = high(Byte) then raise EMaxConstants.create('Max constants reached'); //note since the opcodes is bytes array, it has fixed index size of 256
  inc(FConstantCount);
  result := FOpCodes.Add(ord(OP_CONSTANT));
  FOpCodes.Add(FConstants.Add(Value));
end;

function TChunks.AddDEFINE_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_DEFINE_GLOBAL));
end;

function TChunks.AddDIVIDE: Integer;
begin
  result := FOPCodes.Add(ord(OP_DIVIDE));
end;

function TChunks.AddEQUAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_EQUAL));
end;

function TChunks.AddNotEQUAL: Integer;
begin
  result := AddEqual;
  result := AddNOT;
end;

function TChunks.AddFALSE: Integer;
begin
  result := FOPCodes.Add(ord(OP_FALSE));
end;

function TChunks.AddGET_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_GLOBAL));
end;

function TChunks.AddGET_LOCAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_LOCAL));
end;

function TChunks.AddGET_PROPERTY: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_PROPERTY));
end;

function TChunks.AddGET_SUPER: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_SUPER));
end;

function TChunks.AddGET_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_UPVALUE));
end;

function TChunks.AddGREATER: Integer;
begin
  result := FOPCodes.Add(ord(OP_GREATER));
end;

function TChunks.AddINHERIT: Integer;
begin
  result := FOPCodes.Add(ord(OP_INHERIT));
end;

function TChunks.AddINVOKE: Integer;
begin
  result := FOPCodes.Add(ord(OP_INVOKE));
end;

function TChunks.AddJUMP: Integer;
begin
 result := FOPCodes.Add(ord(OP_JUMP));
end;

function TChunks.AddJUMP_IF_FALSE: Integer;
begin
  result := FOPCodes.Add(ord(OP_JUMP_IF_FALSE));
end;

function TChunks.AddLESS: Integer;
begin
  result := FOPCodes.Add(ord(OP_LESS));
end;

function TChunks.AddLESSTHANEQUAL: integer;
begin
  result := FOPCodes.Add(ord(OP_GREATER));
  result := FOPCodes.Add(ord(OP_NOT));
end;

function TChunks.AddLOOP: Integer;
begin
  result := FOPCodes.Add(ord(OP_LOOP));
end;

function TChunks.AddMETHOD: Integer;
begin
  result := FOPCodes.Add(ord(OP_METHOD));
end;

function TChunks.AddMULTIPLY: Integer;
begin
  result := FOPCodes.Add(ord(OP_MULTIPLY));
end;

function TChunks.AddNEGATE: Integer;
begin
  result := FOPCodes.Add(ord(OP_NEGATE));
end;

function TChunks.AddNIL: Integer;
begin
  result := FOPCodes.Add(ord(OP_NIL));
end;

function TChunks.AddNOT: Integer;
begin
  result := FOPCodes.Add(ord(OP_NOT));
end;

function TChunks.AddPOP: Integer;
begin
  result := FOPCodes.Add(ord(OP_POP));
end;

function TChunks.AddPRINT: Integer;
begin
  result := FOPCodes.Add(ord(OP_PRINT));
end;

function TChunks.AddReturn : integer;
begin
  result := FOPCodes.Add(ord(OP_RETURN));
end;

function TChunks.AddSET_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_GLOBAL));
end;

function TChunks.AddSET_LOCAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_LOCAL));
end;

function TChunks.AddSET_PROPERTY: Integer;
begin
   result := FOPCodes.Add(ord(OP_SET_PROPERTY));
end;

function TChunks.AddSET_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_UPVALUE));
end;

function TChunks.AddSUBTRACT: Integer;
begin
  result := FOPCodes.Add(ord(OP_SUBTRACT));
end;

function TChunks.AddSUPER_INVOKE: Integer;
begin
  result := FOPCodes.Add(ord(OP_SUPER_INVOKE));
end;

function TChunks.AddTRUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_TRUE));
end;

function TChunks.Constants: TDoubles;
begin
  result := FConstants;
end;

procedure TChunks.finalize;
begin
  FOPCodes.Finalize;
  FConstants.Finalize;
end;

procedure TChunks.init;
begin
   FOPCodes.Init;
   FConstants.Init;
end;


{ TInstructionPointer }

function TInstructionPointer.ByteCount: integer;
begin
  result := FBytes.Count;
end;

function TInstructionPointer.Constant(const Index : byte) : pDouble;
begin
  assert((index >= 0) and (index < FConstants.count));
  result := FConstants.Item(Index);
end;

function TInstructionPointer.ConstantCount: integer;
begin
  result := FConstants.Count;
end;

function TInstructionPointer.ByteAt(const Index : integer) : pByte;
begin
  assert((index >= 0) and (index < FBytes.count));
  result := FBytes.Item(FIndex);
end;

function TInstructionPointer.Current: pByte;
begin
  result := ByteAt(FIndex);//TOpCodes(FOPCodes.Item(FIndex)^);
end;

function TInstructionPointer.Index: integer;
begin
  result := FIndex;
end;

procedure TInstructionPointer.Init(const Chunks: TChunks);
begin
  FCurrent := nil;
  FPrevious := nil;
  FBytes := Chunks.FOpCodes;
  FConstants := chunks.FConstants;
  FIndex := -1;
end;

function TInstructionPointer.Next: pByte;
begin
  result := nil;
  if FBytes.count = 0 then exit;

  inc(FIndex);
  if FIndex = FBytes.count then exit;
  result := Current;
end;

function TInstructionPointer.PeekNext: pByte;
var
  i : integer;
begin
  result := nil;
  if FBytes.count = 0 then exit;
  i := FIndex;
  inc(i);
  if i = FBytes.count then exit;
  result :=  FBytes.Item(i);
end;

end.
