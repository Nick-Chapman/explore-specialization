(* v3: based on ./v2.ml
   - program represented via: `op_at_pc : int -> op`
   - main loop now recurses on `pc : int` instead of `op list`
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

let op_at_pc : int -> op = fun pc ->
  let result = 0 in
  let loopc = 1 in
  match pc with
  | 0 -> LOAD_IMMEDIATE (VALUE 0)
  | 1 -> STORE_LOCAL result
  | 2 -> LOAD_IMMEDIATE (VALUE goal)
  | 3 -> STORE_LOCAL loopc

  | 4 -> ADD (result,loopc)
  | 5 -> STORE_LOCAL result
  | 6 -> LOAD_LOCAL loopc
  | 7 -> DEC
  | 8 -> STORE_LOCAL loopc
  | 9 -> JMPNZ (*8*) 4

  | 10 -> PRINT "(Ocaml)Result: "
  | 11 -> LOAD_LOCAL result
  | 12 -> PRINTI
  | 13 -> PRINT "\n"
  | 14 -> HALT
  | _ -> crash "pc to big"

let execute () =
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
  let rec loop pc =
    match op_at_pc pc with
    | LOAD_IMMEDIATE v -> acc := v; loop (pc+1)
    | STORE_LOCAL i -> local_at_put i !acc; loop (pc+1)
    | LOAD_LOCAL i -> acc := local_at i; loop (pc+1)
    | ADD (i,j) -> acc := vadd (local_at i) (local_at j); loop (pc+1)
    | DEC -> acc := vdec (!acc); loop (pc+1)
    | PRINTI -> put_int (deVal (!acc)); loop (pc+1)
    | PRINT s -> put_string s; loop (pc+1)
    | JMPNZ address ->
       if deVal (!acc) = 0
       then loop (pc+1)
       else noinline loop address
    | HALT -> ()
  in
  loop 0

let () = execute ()
