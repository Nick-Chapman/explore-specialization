
let goal = 100_000_000

let printf = Printf.printf

type value = VALUE of int (* TODO: kill this wrapper *)
let vadd (VALUE i) (VALUE j) = VALUE (i+j)
let vdec (VALUE i) = VALUE (i-1)
let deVal (VALUE i) = i

type op =
  | LOAD_IMMEDIATE of value
  | STORE_LOCAL of int
  | LOAD_LOCAL of int
  | ADD of int * int
  | DEC
  | PRINT of string
  | PRINTI
  | JMPNZ of int
  | HALT

let program : op list = (* TODO: move to use array of ops *)
  let (result,loopc) = (0,1) in
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

let rec drop : int -> 'a list -> 'a list = (* TODO: kill when move to array ops *)
  fun i xs ->
  if i = 0 then xs else
  match xs with
  | [] -> []
  | _::xs -> drop (i-1) xs

let execute : op list -> unit =
  fun program0 ->
  let
    setPC : int -> op list = (* TODO: kill when move to array *)
    fun i -> drop i program0
  in
  let zero = VALUE 0 in
  let locals : value array = Array.make 256 zero in
  let local_at_put i v = Array.set locals i v in
  let local_at i = Array.get locals i in
  let acc : value ref = ref zero in
  let rec loop : op list -> unit =
    function
    | [] -> failwith "run out of instructions"
    | op::ops ->
       match op with
       | LOAD_IMMEDIATE v -> acc := v; loop ops
       | STORE_LOCAL i -> local_at_put i !acc; loop ops
       | LOAD_LOCAL i -> acc := local_at i; loop ops
       | ADD (i,j) -> acc := vadd (local_at i) (local_at j); loop ops
       | DEC -> acc := vdec (!acc); loop ops
       | PRINTI -> printf "%d%!" (deVal (!acc)); loop ops
       | PRINT s -> printf "%s%!" s; loop ops
       | JMPNZ address ->
          if deVal (!acc) != 0
          then loop (setPC address)
          else loop ops
       | HALT -> ()
  in
  loop program0

let main() =
  printf "\n";
  let () = execute program in
  ()

let () = main()
