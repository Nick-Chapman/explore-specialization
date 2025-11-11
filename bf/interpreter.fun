
let not b =
  match b with
  | true -> false
  | false -> true

let (>) a b = b < a

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

let chars_of_int = noinline (fun i ->
  let ord0 = ord '0' in
  let char_of_digit c = chr (ord0 + c) in
  let rec loopCofI acc i =
    if i = 0 then acc else
      loopCofI (char_of_digit (i%10) :: acc) (i/10)
  in
  if i = 0 then ['0'] else loopCofI [] i)

let put_int i =
  if i < 0
  then put_chars ('-' :: chars_of_int (0 - i))
  else put_chars (chars_of_int i)

let _debug_put_char c =
  let n = ord c in
  let () = put_string "output: char # " in
  let () = put_int n in
  put_string ".\n"

let _output_char = noinline _debug_put_char
let output_char = noinline put_char

let increment : char -> char = noinline (fun c -> chr ((ord c + 1) % 256))
let decrement : char -> char = noinline (fun c -> chr ((ord c + 255) % 256))

let _debug_step = noinline (fun step pc mp c ->
  let () = put_int step in
  let () = put_string ", pc=" in
  let () = put_int pc in
  let () = put_string ", mp=" in
  let () = put_int mp in
  let () = put_string ", op=" in
  let () = put_char c in
  put_char '\n')

let prog = the_prog

let execute () =

  let max = string_length prog in

  let prog_at : int -> char =  (fun pc ->
    string_index prog pc)
  in

  let mem = make_bytes 30000 in

  let tape_at : int -> char = noinline (fun mp ->
    if mp < 0 then crash "tape_at" else
      get_bytes mem mp)
  in

  let set_tape_at : int -> char -> unit = noinline (fun mp x ->
    if mp < 0 then crash "set_tape_at" else
      set_bytes mem mp x)
  in

  let skip_left : int -> int = fun pc ->
    (*if not (is_comptime_known pc) then crash "unroll-SL" else*)
    let rec skipL nest pc =
      let c = prog_at pc in
      if eq_char c '[' then (if nest = 0 then pc else skipL (nest-1) (pc-1)) else
        if eq_char c ']' then skipL (nest+1) (pc-1) else
          skipL nest (pc-1)
    in
    skipL 0 pc
  in
  let skip_right : int -> int = fun pc ->
    (*if not (is_comptime_known pc) then crash "unroll-SR" else*)
    let rec skipR nest pc =
      let c = prog_at pc in
      if eq_char c ']' then (if nest = 0 then pc else skipR (nest-1) (pc+1)) else
        if eq_char c '[' then skipR (nest+1) (pc+1) else
          skipR nest (pc+1)
    in
    skipR 0 pc
  in

  let rec outer step pc mp =
    let _f () = put_char 'x' in

    let rec inner step pc mp =
      let _f () = put_char '.' in
      let next s pc' =
        if not (is_comptime_known pc) then outer s pc' else
          if pc' < pc then outer s pc' else
            (*if pc' = 2 then outer s 2 else*)
              inner s pc'
      in

      let next' p = next (step+1) p in

      let exec_plus () =
        let c = tape_at mp in
        let () = set_tape_at mp (increment c) in
        next' (pc+1) mp
      in
      let exec_minus () =
        let c = tape_at mp in
        let () = set_tape_at mp (decrement c) in
        next' (pc+1) mp
      in
      let exec_right () =
        next' (pc+1) (mp+1)
      in
      let exec_left () =
        next' (pc+1) (mp-1)
      in
      let exec_left_square () =
        let c = tape_at mp in
        if ord c = 0 then next' (1 + skip_right (pc+1)) mp else next' (pc+1) mp
      in
      let exec_right_square () =
        let c = tape_at mp in
        if ord c = 0 then next' (pc+1) mp else next' (1 + skip_left (pc-1)) mp
      in
      let exec_dot () =
        let c = tape_at mp in
        let () = output_char c in
        next' (pc+1) mp
      in
      let exec_comma () =
        let c = get_char () in
        (*let () = put_string "get_char() -> '" in
        let () = put_char c in
        let () = put_string "'\n" in*)
        let () = set_tape_at mp c in
        next' (pc+1) mp
      in
      let exec_skip _c =
        (*let () = put_string "skipping non-op: '" in
          let () = put_char _c in
          let () = put_string "'\n" in*)
        (*let () = crash "non-op" in*)
        next' (pc+1) mp
      in
      let exec_op op =
        if eq_char op '+' then exec_plus () else
          if eq_char op '-' then exec_minus () else
            if eq_char op '>' then exec_right () else
              if eq_char op '<' then exec_left () else
                if eq_char op '[' then exec_left_square () else
                  if eq_char op ']' then exec_right_square () else
                    if eq_char op '.' then exec_dot () else
                      if eq_char op ',' then exec_comma () else
                        exec_skip op
      in
      (*if not (is_comptime_known pc) then crash "pc is not comptime known" else*)
      (*if pc < 0 then crash "pc<0" else
        if pc > max then crash "pc>max" else*)
          if pc = max then step else (* halt -- return #steps *)
            let op = prog_at pc in
            let _f () = _debug_step step pc mp op in
            exec_op op
    in
(*
    if pc = 0 then inner step 0 mp else
    if pc = 4 then inner step 4 mp else
    if pc = 320 then inner step 320 mp else
    if pc = 339 then inner step 339 mp else
    if pc = 388 then inner step 388 mp else
    if pc = 431 then inner step 431 mp else
    if pc = 434 then inner step 434 mp else
    if pc = 436 then inner step 436 mp else
    if pc = 443 then inner step 443 mp else
    if pc = 453 then inner step 453 mp else
    if pc = 498 then inner step 498 mp else

    if pc = 188 then inner step 188 mp else
    if pc = 198 then inner step 198 mp else
    if pc = 231 then inner step 231 mp else
    if pc = 250 then inner step 250 mp else
    if pc = 274 then inner step 274 mp else
    if pc = 292 then inner step 292 mp else
    if pc = 306 then inner step 306 mp else
    if pc = 310 then inner step 310 mp else
    if pc = 456 then inner step 456 mp else
    if pc = 501 then inner step 501 mp else
 *)

    (*if pc = 0 then inner step 0 mp else
    if pc = 2 then inner step 2 mp else
      let () = put_string "\n**outer,need spec: pc=" in
        let () = put_int pc in
        let () = put_string "\n" in
        crash "outer"*)
    inner step pc mp

  in outer 0 0 0

let execute = noinline execute

let main() =
  let () = put_string "mode:" in
  let () = put_string the_mode in
  let () = put_string "\n" in
  let () = put_string "eval:" in
  let _f () = put_string the_prog in
  let () = put_string "\n" in
  let steps = execute () in
  let () = put_string "#steps=" in
  put_int steps;
  let () = put_string "\n" in
  ()
