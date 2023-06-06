unit Chunk;

interface
uses Loxtypes;


(* Taken from crafting interpreters pp398 grow array - this hopefully mimics the c code close enough
    1.Allocate a new array with more capacity.
    2.Copy the existing elements from the old array to the new one.
    3.Store the new capacity.
    4.Delete the old array.
    5.Update code to point to the new array.
    6.Store the element in the new array now that there is room.
    7.Update the count.
*)
(*Note : obviously, you don't need to create your own dynamic arrays in delphi, but it's kind of fun to do it yourself.
  Haha. Yeah, right.


*)

const
   MIN_CAPACITY = 8;

type

 TDynamicByteArray = record
    index       : integer;
    count       : integer;
    oldcapacity : integer;
    capacity    : integer;
    pCode       : pByte;
    function Add(const value : byte) : integer;
    function  Byte(const index : integer) : pByte;
    procedure AllocateArray;
    procedure init;
    procedure finalize;
    function  AllocateNewArrayWithMoreCapacity : boolean;
    Procedure CopyExistingElementsFromOldArrayToNewArray;
    procedure GrowCapacity;
 end;


 TChunk = record
   OPCodes      : TDynamicByteArray;
   ConstantPool : TDynamicByteArray;
   procedure init;
   procedure finalize;
 end;


implementation


procedure TDynamicByteArray.AllocateArray;
begin
  GetMem(pCode,Capacity);
  fillchar(pCode^,Capacity,#0);
end;

function TDynamicByteArray.AllocateNewArrayWithMoreCapacity : boolean;
begin
  result := false;
  if pCode = nil then exit;
  growCapacity;
  CopyExistingElementsFromOldArrayToNewArray;
  result := true;
end;


function TDynamicByteArray.Add(const value : byte) : integer;
var
  p : pByte;
begin
  result := -1;
  inc(count);
  if (count) > Capacity then
  begin
    if not AllocateNewArrayWithMoreCapacity then exit;
  end;
  p := Byte(Count-1);
  if p <> nil then
  begin
    p^ := value;
    result := Count-1;
  end;
end;

function TDynamicByteArray.Byte(const index: integer): pByte;
begin
  result := nil;
  if (index <= capacity) and (index >= 0) then
  begin
    result := Pcode;
    inc(result,index);
  end;
end;

procedure TDynamicByteArray.CopyExistingElementsFromOldArrayToNewArray;
var
  p : pByte;
begin
  GetMem(p,Capacity);             //get new memory
  fillchar(p^,Capacity,#0);       //initialise new memory as 0
  Move(pCode^, p^, oldcapacity);  //copy existing memory into new memory;
  FreeMem(pCode);                 //free the old memory
  pCode := nil;                   //make the old memory nil
  pCode := p;                     // set the old memory to the new memory;
end;


procedure TDynamicByteArray.finalize;
begin
  if assigned(pCode) then
  begin
    freeMem(pCode);
    pCode := nil;
  end;
end;


procedure TDynamicByteArray.growCapacity;
begin
  oldcapacity := capacity;
  if capacity = 0 then
  begin
    capacity := MIN_CAPACITY;
  end
  else
  begin
    capacity := capacity  * 2;
  end;
end;

procedure TDynamicByteArray.init;
begin
  index := 0;
  count := 0;
  capacity := MIN_CAPACITY;
  pcode := nil;
  AllocateArray;
end;







{ TChunk }
procedure TChunk.finalize;
begin
  OPCodes.Finalize;
  ConstantPool.Finalize;
end;

procedure TChunk.init;
begin
   OPCodes.Init;
   ConstantPool.Init;
end;

end.
