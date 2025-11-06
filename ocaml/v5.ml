(* v5: based on ./v4.ml
   - avoid mutable state for accumulator
 *)

let goal = 5 (*10_000_000*)

let crash = failwith
let noinline x = x
let put_char = Printf.printf "%c"
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

  (* continuation style to get case-of-case optimization *)
  let with_starting pc k =
    if pc = 0 then k 0 else
      if pc = 4 then k 4 else
        crash "with_starting"
  in

  let rec outer acc pc =
    let () = put_char 'x' in
    let inner = unroll (fun inner acc pc ->
      let () = put_char '.' in
      let loop acc pc' =
        let backedge = not (pc < pc') in
        let jump_dest = (pc' = 4) in (*the only jump dest*)
        (if backedge || jump_dest then outer else inner) acc pc'
      in
      match op_at_pc pc with
      | LOAD_IMMEDIATE v -> let acc = v in loop acc (pc+1)
      | STORE_LOCAL i -> local_at_put i acc; loop acc (pc+1)
      | LOAD_LOCAL i -> let acc = local_at i in loop acc (pc+1)
      | ADD (i,j) -> let acc = vadd (local_at i) (local_at j) in loop acc (pc+1)
      | DEC -> let acc = vdec acc in loop acc (pc+1)
      | PRINTI -> put_int (deVal acc); loop acc (pc+1)
      | PRINT s -> put_string s; loop acc (pc+1)
      | JMPNZ address ->
         if deVal acc = 0
         then loop acc (pc+1)
         else loop acc address
      | HALT -> ())
    in
    with_starting pc (inner acc)
  in
  outer zero 0

let main() =
  let () = execute () in
  ()


let () = main ()
