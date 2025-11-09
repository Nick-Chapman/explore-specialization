
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

let prim_LessInt(a,b) = ((a:int) < b)
let prim_AddInt(a,b) = a + b
let prim_SubInt(a,b) = a - b
let prim_EqInt(a,b) = ((a:int) = b)

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
(*Stage2 (NbE)*)
let explode =
  (fun (s) ->
    let explode_loop =
      fix (fun explode_loop (acc,i) ->
        (match prim_LessInt(i, 0) with
        | true -> acc
        | false ->
          let x = prim_StringIndex(s, i) in
          explode_loop (Cons(x, acc), prim_SubInt(i, 1)))) in
    let x = prim_StringLength(s) in
    explode_loop (Nil, prim_SubInt(x, 1))) in
let put_chars =
  fix (fun put_chars (xs) ->
    (match xs with
    | Nil -> Unit
    | Cons(x,xs) ->
      let _ = prim_PutChar(x) in
      put_chars (xs))) in
let put_int =
  (fun (i) ->
    (match prim_LessInt(i, 0) with
    | true ->
      let i = prim_SubInt(0, i) in
      let loop =
        fix (fun loop (acc,i) ->
          (match prim_EqInt(i, 0) with
          | true -> acc
          | false ->
            let c = prim_ModInt(i, 10) in
            let x = prim_AddInt(48, c) in
            let x = prim_CharChr(x) in
            loop (Cons(x, acc), prim_DivInt(i, 10)))) in
      let y =
        (match prim_EqInt(i, 0) with
        | true -> Cons('0', Nil)
        | false -> loop (Nil, i)) in
      put_chars (Cons('-', y))
    | false ->
      let loop =
        fix (fun loop (acc,i) ->
          (match prim_EqInt(i, 0) with
          | true -> acc
          | false ->
            let c = prim_ModInt(i, 10) in
            let x = prim_AddInt(48, c) in
            let x = prim_CharChr(x) in
            loop (Cons(x, acc), prim_DivInt(i, 10)))) in
      put_chars
      ((match prim_EqInt(i, 0) with
      | true -> Cons('0', Nil)
      | false -> loop (Nil, i))))) in
let rec outer =
  (fun (pc,acc,locals) ->
    (match prim_EqInt(pc, 0) with
    | true ->
      let locals =
        (match locals with
        | Locals(local0,local1) -> Locals(VALUE(0), local1)) in
      let locals =
        (match locals with
        | Locals(local0,local1) -> Locals(local0, VALUE(100000000))) in
      outer (4, VALUE(100000000), locals)
    | false ->
      (match prim_EqInt(pc, 4) with
      | true ->
        let v1 =
          (match locals with
          | Locals(local0,local1) -> local0) in
        let v2 =
          (match locals with
          | Locals(local0,local1) -> local1) in
        let x =
          (match v1 with
          | VALUE(i) -> i) in
        let y =
          (match v2 with
          | VALUE(i) -> i) in
        let x = prim_AddInt(x, y) in
        let locals =
          (match locals with
          | Locals(local0,local1) -> Locals(VALUE(x), local1)) in
        let acc =
          (match locals with
          | Locals(local0,local1) -> local1) in
        let x =
          (match acc with
          | VALUE(i) -> i) in
        let x = prim_SubInt(x, 1) in
        let locals =
          (match locals with
          | Locals(local0,local1) -> Locals(local0, VALUE(x))) in
        (match prim_EqInt(x, 0) with
        | true ->
          let _ = put_chars (explode ("(Ocaml)Result: ")) in
          let acc =
            (match locals with
            | Locals(local0,local1) -> local0) in
          let _ =
            put_int
            ((match acc with
            | VALUE(i) -> i)) in
          let _ = put_chars (explode ("\n")) in
          Unit
        | false -> outer (4, VALUE(x), locals))
      | false -> prim_Crash("with_starting")))) in
outer (0, VALUE(0), Locals(VALUE(0), VALUE(0)))

let Unit = main()
