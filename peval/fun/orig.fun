(*v4*)

let goal = 100000000

let not b =
  match b with
  | true -> false
  | false -> true

let explode = noinline (fun s ->
  let rec explode_loop acc i =
    if i < 0 then acc else
      explode_loop (string_index s i :: acc) (i-1)
  in
  explode_loop [] (string_length s - 1))

let rec put_chars xs =
  match xs with
  | [] -> ()
  | x::xs -> put_char x; put_chars xs

let put_string s = put_chars (explode s)

let chars_of_int i =
  let ord0 = ord '0' in
  let char_of_digit c = chr (ord0 + c) in
  let rec loop acc i =
    if i = 0 then acc else
      loop (char_of_digit (i%10) :: acc) (i/10)
  in
  if i = 0 then ['0'] else loop [] i

let put_int = noinline (fun i ->
  if i < 0
  then put_chars ('-' :: chars_of_int (0 - i))
  else put_chars (chars_of_int i))


type value = VALUE of int
let deVal v = match v with VALUE i -> i
let mkVal x = VALUE x

(*type value = int
let mkVal v = v
let deVal v = v*)

let vadd v1 v2 = mkVal (deVal v1 + deVal v2)
let vdec v = mkVal (deVal v - 1)

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

  if pc = 0 then LOAD_IMMEDIATE (mkVal 0) else
  if pc = 1 then STORE_LOCAL result else
  if pc = 2 then LOAD_IMMEDIATE (mkVal goal) else
  if pc = 3 then STORE_LOCAL loopc else

  if pc = 4 then ADD (result,loopc) else
  if pc = 5 then STORE_LOCAL result else
  if pc = 6 then LOAD_LOCAL loopc else
  if pc = 7 then DEC else
  if pc = 8 then STORE_LOCAL loopc else
  if pc = 9 then JMPNZ (*8*) 4 else

  if pc = 10 then PRINT "(Ocaml)Result: " else
  if pc = 11 then LOAD_LOCAL result else
  if pc = 12 then PRINTI else
  if pc = 13 then PRINT "\n" else
  if pc = 14 then HALT else
  crash "pc to big"

type locals = Locals of (value*value)

let local_at_put : locals -> int -> value -> locals = fun locals i v ->
  match locals with Locals (local0,local1) ->
    if i = 0 then Locals (v,local1) else
      if i = 1 then Locals (local0,v) else
        crash "local_at_put"

let local_at : locals -> int -> value = fun locals i ->
  match locals with Locals (local0,local1) ->
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
    (*let () = put_char 'x' in*)
    let [@unroll] rec inner pc acc locals =
      (*let () = put_char '.' in*)
      let loop pc' =
       if not (is_comptime_known pc') then outer pc' else
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
      | HALT -> ()
    in
    with_starting pc (fun pc -> inner pc acc locals)
  in

  let zero = mkVal 0 in
  outer 0 zero (Locals (zero,zero))
