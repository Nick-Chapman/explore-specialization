
let put_char : char -> unit = fun c ->
    let open Printf in
    let n = Char.code c in
    let printable = n >= 32 && n <= 126 in
    let nl = (n == 10) in
    let bs = (n == 8) in
    if printable || nl || bs then printf "%c%!" c else printf "\\%02x%!" n

let fix : ('a -> 'a) -> 'a = fun f ->
  let rec fixed x = f fixed x
  in fixed

type 'a myList = Nil | Cons of 'a * 'a myList
type myUnit = Unit

let prim_LessInt(a,b) = a < b
let prim_AddInt(a,b) = a + b
let prim_SubInt(a,b) = a - b
let prim_EqInt(a,b) = a = b
let prim_DivInt(a,b) = a / b
let prim_ModInt(a,b) = a mod b
let prim_StringIndex (s,i) = s.[i]
let prim_StringLength s = String.length s
let prim_CharOrd c = Char.code c
let prim_CharChr n = Char.chr (n mod 256)
let prim_Crash = failwith
let prim_PutChar c = put_char c


type value = VALUE of int
(*type value = int*)
type locals = Locals of (value*value)

type op
  = LOAD_IMMEDIATE of value
  | STORE_LOCAL of int
  | LOAD_LOCAL of int
  | ADD of int * int
  | DEC
  | PRINT of string
  | PRINTI
  | JMPNZ of int
  | HALT

let main () =
