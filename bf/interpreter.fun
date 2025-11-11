
let not b =
  match b with
  | true -> false
  | false -> true

let (>) a b = b < a

let rec length xs =
  match xs with
  | [] -> 0
  | _::xs -> 1 + length xs

let rec rev_onto acc xs =
  match xs with
  | [] -> acc
  | x::xs -> rev_onto (x :: acc) xs

let rev xs = rev_onto [] xs

let (@) xs ys = rev_onto ys (rev xs)

let explode = noinline (fun s ->
  let rec explode_loop acc i =
    if i < 0 then acc else
      explode_loop (string_index s i :: acc) (i-1)
  in
  explode_loop [] (string_length s - 1))

let implode = noinline (fun xs ->
  let b = make_bytes (length xs) in
  let rec loop i xs =
    match xs with
    | [] -> ()
    | x::xs -> set_bytes b i x; loop (i+1) xs
  in
  loop 0 xs;
  freeze_bytes b)

let concat = noinline (fun s1 s2 ->
  implode (explode s1 @ explode s2))

let (^) = concat

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

let sofi = noinline (fun i -> implode (chars_of_int i))

let put_int i =
  if i < 0
  then put_chars ('-' :: chars_of_int (0 - i))
  else put_chars (chars_of_int i)

let _debug_put_char c =
  let n = ord c in
  let () = put_string "output: char # " in
  let () = put_int n in
  put_string ".\n"

let output_char = noinline _debug_put_char
let _output_char = noinline put_char

let _increment : char -> char = noinline (fun c -> chr ((ord c + 1) % 256))
let _decrement : char -> char = noinline (fun c -> chr ((ord c + 255) % 256))

let _debug_step = noinline (fun _pc _step _mp _c ->
  let _no () =
    let () = put_int _step in
    let () = put_string ", pc=" in
    let () = put_int _pc in
    let () = put_string ", mp=" in
    let () = put_int _mp in
    let () = put_string ", op=" in
    let () = put_char _c in
    put_char '\n'
  in ())

let prog = the_prog



let need_case pc =
  let () = put_string "\n** cases: need case for:" in
  let () = put_int pc in
  let () = put_string "\n" in
  crash "need_case"

let cases xs pc k =
  let [@unroll] rec loop xs = match xs with
    | [] -> need_case pc
    | x::xs -> if pc = x then k x else loop xs
  in
  loop xs

let zero_upto n =
  let [@unroll] rec loop acc n =
    if n < 0 then acc else loop (n::acc) (n-1)
  in
  loop [] n


let execute () =

  let max = string_length prog in

  put_string ("max = " ^ sofi max ^ "\n");

  let prog_at : int -> char =  (fun pc ->
    string_index prog pc)
  in

  let skip_left : int -> int = fun pc ->
   if not (is_comptime_known pc) then crash "skip_left" else
    let [@unroll] rec skipL nest pc =
      let c = prog_at pc in
      if eq_char c '[' then (if nest = 0 then pc else skipL (nest-1) (pc-1)) else
        if eq_char c ']' then skipL (nest+1) (pc-1) else
          skipL nest (pc-1)
    in
    skipL 0 pc
  in
  let skip_right : int -> int = fun pc ->
   if not (is_comptime_known pc) then crash "skip_right" else
    let [@unroll] rec skipR nest pc =
      let c = prog_at pc in
      if eq_char c ']' then (if nest = 0 then pc else skipR (nest-1) (pc+1)) else
        if eq_char c '[' then skipR (nest+1) (pc+1) else
          skipR nest (pc+1)
    in
    skipR 0 pc
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

  let dispatch step mp next pc =
    let exec_plus () =
      let c = tape_at mp in
      let () = set_tape_at mp (_increment c) in
      next (pc+1) mp
    in
    let exec_minus () =
      let c = tape_at mp in
      let () = set_tape_at mp (_decrement c) in
      next (pc+1) mp
    in
    let exec_right () =
      next (pc+1) (mp+1)
    in
    let exec_left () =
      next (pc+1) (mp-1)
    in
    let exec_left_square () =
      let c = tape_at mp in
      if ord c = 0 then next (1 + skip_right (pc+1)) mp else next (pc+1) mp
    in
    let exec_right_square () =
      let c = tape_at mp in
      if ord c = 0 then next (pc+1) mp else next (1 + skip_left (pc-1)) mp
    in
    let exec_dot () =
      let c = tape_at mp in
      let () = output_char c in
      next (pc+1) mp
    in
    let exec_comma () =
      let c = get_char () in
      let _no () =
        let () = put_string "get_char() -> '" in
        let () = put_char c in
        let () = put_string "'\n" in
        ()
      in
      let () = set_tape_at mp c in
      next (pc+1) mp
    in
    let exec_skip _c =
      next (pc+1) mp
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
    (* TODO: move halt condition to caller; so no need to pass step *)
    if pc = max then step else (* halt -- return #steps *)
      let op = prog_at pc in
      let () = _debug_step pc step mp op in
      exec_op op
  in

  let _all_pcs = zero_upto max in
  let selected_pcs = [0;3;8] in (* TODO: determine automatically *)

  let rec outer pc step mp =
    let _no () = put_char 'x' in
    cases selected_pcs pc (fun pc ->
        let [@unroll] rec inner pc step mp =
          let _no () = put_char '.' in
          let next pc' mp =
            (*if not (is_comptime_known pc) then crash "?pc" else*)
            let backedge = pc' < pc in
            if backedge
            then outer pc' (step+1) mp
            else inner pc' (step+1) mp
          in
          dispatch step mp next pc
        in
        inner pc step mp
      )
  in

  outer 0 0 0

let execute = noinline execute

let main() =
  let () = put_string "mode:" in
  let () = put_string the_mode in
  let () = put_string "\n" in
  let _no () = put_string "eval:" in
  let () = put_string the_prog in
  let () = put_string "\n" in
  let steps = execute () in
  let () = put_string "#steps=" in
  put_int steps;
  let () = put_string "\n" in
  ()
