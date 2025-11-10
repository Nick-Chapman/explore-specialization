
let noinline x = x
let string_index s i = prim_StringIndex (s,i)
let string_length s = prim_StringLength s
let ord = prim_CharOrd
let chr = prim_CharChr
let (%) a b = prim_ModInt (a,b)
let crash = prim_Crash
