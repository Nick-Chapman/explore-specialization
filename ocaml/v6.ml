(* v5: based on ./v5.ml
   - avoid mutable state for locals
 *)

let goal = 100

let crash = failwith
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

(* Just enough steps of unrolling... *)
let unroll _f _x = crash "unroll"
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x
let unroll f x = f (unroll f) x

type locals = (value*value)

let local_at_put : locals -> int -> value -> locals = fun (local0,local1) i v ->
  if i = 0 then (v,local1) else
    if i = 1 then (local0,v) else
      crash "local_at_put"

let local_at : locals -> int -> value = fun (local0,local1) i ->
  if i = 0 then local0 else
    if i = 1 then local1 else
      crash "local_at"

(* continuation style to get case-of-case optimization *)
let with_starting pc k =
  if pc = 0 then k 0 else
    if pc = 4 then k 4 else
      crash "with_starting"

let main () =
  let rec outer pc acc locals =
    let inner = unroll (fun inner pc acc locals ->
      let loop pc' =
        let backedge = not (pc < pc') in
        let jump_dest = (pc' = 4) in (*the only jump dest*)
        (if backedge || jump_dest then outer else inner) pc'
      in
      match op_at_pc pc with
      | LOAD_IMMEDIATE v -> let acc = v in loop (pc+1) acc locals
      | STORE_LOCAL i -> let locals = local_at_put locals i acc in loop (pc+1) acc locals
      | LOAD_LOCAL i -> let acc = local_at locals i in loop (pc+1) acc locals
      | ADD (i,j) -> let acc = vadd (local_at locals i) (local_at locals j) in loop (pc+1) acc locals
      | DEC -> let acc = vdec acc in loop (pc+1) acc locals
      | PRINTI -> put_int (deVal acc); loop (pc+1) acc locals
      | PRINT s -> put_string s; loop (pc+1) acc locals
      | JMPNZ address ->
         if deVal acc = 0
         then loop (pc+1) acc locals
         else loop address acc locals
      | HALT -> ())
    in
    with_starting pc (fun pc -> inner pc acc locals)
  in

  let zero = VALUE 0 in
  outer 0 zero (zero,zero)

let () = main ()
