unit OpCodes;

interface

const
 
        OP_NULL = 0;
        OP_CALL= 1;
        OP_TRUE= 2;
        OP_FALSE=3 ;
        OP_POP= 4;
        OP_GET_LOCAL=5 ;
        OP_SET_LOCAL=6;
        OP_GET_GLOBAL=7 ;
        OP_DEFINE_GLOBAL=8 ;
        OP_SET_GLOBAL=9;
        OP_GET_UPVALUE=10 ;
        OP_SET_UPVALUE=11 ;
        OP_GET_PROPERTY=12 ;
        OP_SET_PROPERTY=13 ;
        OP_GET_SUPER=14 ;
        OP_EQUAL=15 ;
        OP_GREATER=16 ;
        OP_LESS= 17;
        OP_ADD= 18;
        OP_SUBTRACT= 19;
        OP_MULTIPLY= 20;
        OP_DIVIDE= 21;
        OP_NOT= 22;
        OP_NEGATE= 23;
        OP_PRINT= 24;
        OP_JUMP= 25;
        OP_JUMP_IF_FALSE= 26;
        OP_LOOP= 27;
        OP_NIL= 28;
        OP_INVOKE= 29;
        OP_SUPER_INVOKE= 30;
        OP_CLOSURE= 31;
        OP_CLOSE_UPVALUE= 32;
        OP_RETURN= 33;
        OP_CLASS= 34;
        OP_INHERIT= 35;
        OP_METHOD= 36;
        OP_CONSTANT= 37;
        OP_BUILD_LIST= 38;
        OP_INDEX_SUBSCR= 39;
        OP_STORE_SUBSCR= 40;

implementation




end.
