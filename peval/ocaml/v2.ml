(* v2: based on ./peval.ml made to work in barefun
   - still has list of structured opcodes
   - value still a lifted types
   - accumulator still a ref value
   - locals: now a pair of ref values (only 2 locals supported!)
   - main loop: recursive knot tied with user-defined "myfix"
   - some hand unrolling of myfix.
   - printing achieved via put_{char,string,int}
 *)

let goal = 100_000_000

let crash = failwith
let noinline x = x
let put_int = Printf.printf "%d"
let put_string = Printf.printf "%s"

type value = VALUE of int
let deVal v = match v with VALUE i -> i
let vadd v1 v2 = VALUE (deVal v1 + deVal v2)
let vdec v = VALUE (deVal v - 1)

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

let program : op list =
  let result = 0 in
  let loopc = 1 in
  [ LOAD_IMMEDIATE (VALUE 0)
  ; STORE_LOCAL result
  ; LOAD_IMMEDIATE (VALUE goal)
  ; STORE_LOCAL loopc

  ; ADD (result,loopc)
  ; STORE_LOCAL result
  ; LOAD_LOCAL loopc
  ; DEC
  ; STORE_LOCAL loopc
  ; JMPNZ (*8*) 4

  ; PRINT "(Ocaml)Result: "
  ; LOAD_LOCAL result
  ; PRINTI
  ; PRINT "\n"
  ; HALT
  ]

let rec drop : int -> 'a list -> 'a list =
  fun i xs ->
  if i = 0 then xs else
  match xs with
  | [] -> []
  | _::xs -> drop (i-1) xs

let myfix : ('a -> 'a) -> 'a = fun f ->
  let rec fixed x = f fixed x
  in fixed

let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x
let myfix : ('a -> 'a) -> 'a = fun f x -> f (myfix f) x

let execute : op list -> unit =
  fun program0 ->
  let setPC : int -> op list = fun i -> drop i program0 in
  let zero = VALUE 0 in
  let local0 = ref zero in
  let local1 = ref zero in
  let local_at_put : int -> value -> unit = noinline (fun i v ->
    if i = 0 then local0 := v else
      if i = 1 then local1 := v else
        crash "local_at_put")
  in
  let local_at : int -> value = noinline (fun i ->
    if i = 0 then !local0 else
      if i = 1 then !local1 else
        crash "local_at")
  in
  let acc : value ref = ref zero in
  let loop = myfix (fun loop -> fun ops ->
    match ops with
    | [] -> crash "run out of instructions"
    | op::ops ->
       match op with
       | LOAD_IMMEDIATE v -> acc := v; loop ops
       | STORE_LOCAL i -> local_at_put i !acc; loop ops
       | LOAD_LOCAL i -> acc := local_at i; loop ops
       | ADD (i,j) -> acc := vadd (local_at i) (local_at j); loop ops
       | DEC -> acc := vdec (!acc); loop ops
       | PRINTI -> put_int (deVal (!acc)); loop ops
       | PRINT s -> put_string s; loop ops
       | JMPNZ address ->
          if deVal (!acc) = 0
          then loop ops
          else noinline loop (setPC address)
       | HALT -> ())
  in
  loop program0

let () = execute program
