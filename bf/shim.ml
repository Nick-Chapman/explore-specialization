
type 'a myList = Nil | Cons of 'a * 'a myList
type myUnit = Unit

let prim_CharChr a = chr a
let prim_CharOrd a = ord a
let prim_AddInt(a,b) = a + b
let prim_SubInt(a,b) = a - b
let prim_ModInt(a,b) = a % b
let prim_DivInt(a,b) = a / b
let prim_EqInt(a,b) = a = b
let prim_LessInt(a,b) = a < b
let prim_StringIndex (s,i) = string_index s i
let prim_StringLength a = string_length a
let prim_PutChar a = put_char a
let prim_EqChar (a,b) = eq_char a b
let prim_GetChar Unit = get_char ()
let prim_MakeBytes a = make_bytes a
let prim_GetBytes (b,i) = get_bytes b i
let prim_SetBytes (b,i,x) = set_bytes b i x
let prim_Crash a = crash a
let prim_FreezeBytes a = freeze_bytes a
