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
    function  AddConstant(const Value: Double) : Integer;
    function  AddReturn : integer;
    procedure init;
    procedure finalize;
  end;


implementation

uses
  sysutils;


//add the opcode constant to the opcodes, add a constant to the constants, then add the index of the contant to the opcodes.
function TChunk.AddConstant(const Value: Double) : Integer;
begin
  if FConstantCount = high(Byte) then raise exception.create('Max constants reached'); //note since the opcodes is bytes array, it has fixed index size of 256
  inc(FConstantCount);
  result := FOpCodes.Add(ord(OP_CONSTANT));
  FOpCodes.Add(FConstants.Add(Value));
end;

function TChunk.AddReturn : integer;
begin
  result := FOPCodes.Add(ord(OP_RETURN));
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


end.
