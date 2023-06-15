unit Chunk;

interface

uses
  Loxtypes,
  DoubleArray,
  IntegerArray,
  ByteArray;

type


  TChunk = record
  private
    FConstantCount : integer;
    FOpCodes   : TBytes;
    FConstants : TDoubles;
    // FLine      : TIntegers;
  public
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
    Function AddGREATER : Integer;
    Function AddLESS : Integer;
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
  TChunkIterator = record
  private
    FOpCodes   : TBytes;
    FIndex : integer;
    FCurrent : TOpCodes;
  public
    function Current : TOpCodes;
    function Next : TOpCodes;
    function PeekNext : TOpCodes;
    procedure Init(const Chunk: TChunk);
  end;

implementation
uses
  Exceptions;


//add the opcode constant to the opcodes, add a constant to the constants, then add the index of the contant to the opcodes.
function TChunk.AddADD: Integer;
begin
  result := FOPCodes.Add(ord(OP_ADD));
end;

function TChunk.AddCALL: Integer;
begin
 result := FOPCodes.Add(ord(OP_CALL));
end;

function TChunk.AddCLASS: Integer;
begin
 result := FOPCodes.Add(ord(OP_CLASS));
end;

function TChunk.AddCLOSE_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_CLOSE_UPVALUE));
end;

function TChunk.AddCLOSURE: Integer;
begin
  result := FOPCodes.Add(ord(OP_CLOSURE));
end;

function TChunk.AddConstant(const Value: Double) : Integer;
begin
  if FConstantCount = high(Byte) then raise EMaxConstants.create('Max constants reached'); //note since the opcodes is bytes array, it has fixed index size of 256
  inc(FConstantCount);
  result := FOpCodes.Add(ord(OP_CONSTANT));
  FOpCodes.Add(FConstants.Add(Value));
end;

function TChunk.AddDEFINE_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_DEFINE_GLOBAL));
end;

function TChunk.AddDIVIDE: Integer;
begin
  result := FOPCodes.Add(ord(OP_DIVIDE));
end;

function TChunk.AddEQUAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_EQUAL));
end;

function TChunk.AddFALSE: Integer;
begin
  result := FOPCodes.Add(ord(OP_FALSE));
end;

function TChunk.AddGET_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_GLOBAL));
end;

function TChunk.AddGET_LOCAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_LOCAL));
end;

function TChunk.AddGET_PROPERTY: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_PROPERTY));
end;

function TChunk.AddGET_SUPER: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_SUPER));
end;

function TChunk.AddGET_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_GET_UPVALUE));
end;

function TChunk.AddGREATER: Integer;
begin
  result := FOPCodes.Add(ord(OP_GREATER));
end;

function TChunk.AddINHERIT: Integer;
begin
  result := FOPCodes.Add(ord(OP_INHERIT));
end;

function TChunk.AddINVOKE: Integer;
begin
  result := FOPCodes.Add(ord(OP_INVOKE));
end;

function TChunk.AddJUMP: Integer;
begin
 result := FOPCodes.Add(ord(OP_JUMP));
end;

function TChunk.AddJUMP_IF_FALSE: Integer;
begin
  result := FOPCodes.Add(ord(OP_JUMP_IF_FALSE));
end;

function TChunk.AddLESS: Integer;
begin
  result := FOPCodes.Add(ord(OP_LESS));
end;

function TChunk.AddLOOP: Integer;
begin
  result := FOPCodes.Add(ord(OP_LOOP));
end;

function TChunk.AddMETHOD: Integer;
begin
  result := FOPCodes.Add(ord(OP_METHOD));
end;

function TChunk.AddMULTIPLY: Integer;
begin
  result := FOPCodes.Add(ord(OP_MULTIPLY));
end;

function TChunk.AddNEGATE: Integer;
begin
  result := FOPCodes.Add(ord(OP_NEGATE));
end;

function TChunk.AddNIL: Integer;
begin
  result := FOPCodes.Add(ord(OP_NIL));
end;

function TChunk.AddNOT: Integer;
begin
  result := FOPCodes.Add(ord(OP_NOT));
end;

function TChunk.AddPOP: Integer;
begin
  result := FOPCodes.Add(ord(OP_POP));
end;

function TChunk.AddPRINT: Integer;
begin
  result := FOPCodes.Add(ord(OP_PRINT));
end;

function TChunk.AddReturn : integer;
begin
  result := FOPCodes.Add(ord(OP_RETURN));
end;

function TChunk.AddSET_GLOBAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_GLOBAL));
end;

function TChunk.AddSET_LOCAL: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_LOCAL));
end;

function TChunk.AddSET_PROPERTY: Integer;
begin
   result := FOPCodes.Add(ord(OP_SET_PROPERTY));
end;

function TChunk.AddSET_UPVALUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_SET_UPVALUE));
end;

function TChunk.AddSUBTRACT: Integer;
begin
  result := FOPCodes.Add(ord(OP_SUBTRACT));
end;

function TChunk.AddSUPER_INVOKE: Integer;
begin
  result := FOPCodes.Add(ord(OP_SUPER_INVOKE));
end;

function TChunk.AddTRUE: Integer;
begin
  result := FOPCodes.Add(ord(OP_TRUE));
end;

procedure TChunk.finalize;
begin
  FOPCodes.Finalize;
  FConstants.Finalize;
end;

procedure TChunk.init;
begin
   FOPCodes.Init;
   FConstants.Init;
end;


{ TChunkIterator }

function TChunkIterator.Current: TOpCodes;
begin
  result := TOpCodes(FOPCodes.Item(FIndex)^);
end;

procedure TChunkIterator.Init(const Chunk: TChunk);
begin
  FOpCodes := Chunk.FOpCodes;
  FIndex := -1;
end;

function TChunkIterator.Next: TOpCodes;
begin
  result := OP_NULL;
  if (current = OP_CONSTANT) then inc(FIndex); //skip to actual contant index; We will then skip past it one more time
  inc(FIndex);
  if FIndex = FOpCodes.count then exit;
  result := Current;
end;

function TChunkIterator.PeekNext: TOpCodes;
var
  i : integer;
begin
  result := OP_NULL;
  i := FIndex;
  if (current = OP_CONSTANT) then inc(i); //skip to actual contant index; We will then skip past it one more time
  inc(i);
  if i = FOpCodes.count then exit;
  result := TOpCodes(FOPCodes.Item(i)^);
end;

end.
