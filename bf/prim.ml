
exception CRASH of string

module X : sig

  val crash : string -> 'a

  val noinline : 'a -> 'a

  val is_comptime_known : 'a -> bool

  val ( * ) : int -> int -> int
  val (%) : int -> int -> int
  val (+) : int -> int -> int
  val (-) : int -> int -> int
  val (/) : int -> int -> int
  val (<) : int -> int -> bool
  val (=) : int -> int -> bool

  val (||) : bool -> bool -> bool
  val (&&) : bool -> bool -> bool

  val chr : int -> char
  val eq_char : char -> char -> bool
  val get_char : unit -> char
  val ord : char -> int
  val put_char : char -> unit

  type 'a ref
  val ref : 'a -> 'a ref
  val (!) : 'a ref -> 'a
  val (:=) : 'a ref -> 'a -> unit

  type bytes
  val make_bytes : int -> bytes
  val set_bytes : bytes -> int -> char -> unit
  val get_bytes : bytes -> int -> char
  val freeze_bytes : bytes  -> string
  val thaw_bytes : string -> bytes
  val string_index : string -> int -> char
  val string_length : string -> int

  val load_sector : int -> bytes -> unit
  val store_sector : int -> string -> unit

  val free_words : unit -> int

  val init_interrupt_mode : unit -> unit
  val get_ticks : unit -> int
  val wait_for_interrupt : unit -> unit
  val is_keyboard_ready : unit -> bool
  val get_keyboard_last_scancode : unit -> char

end = struct

  let crash message = raise (CRASH message)

  let noinline x = x

  let is_comptime_known _ = true

  let (=) = (=)
  let (<) = (<)
  let ( * ) = ( * )
  let (+) = (+)
  let (-) = (-)
  let (%) = (mod)
  let (/) = (/)

  let (||) = (||)
  let (&&) = (&&)

  let ord = Char.code
  let chr n = Char.chr (n % 256)

  let eq_char = (=)
  let get_char () =
    let bytes = Bytes.create 1 in
    let n = Unix.read Unix.stdin bytes 0 1 in
    if n < 1 then (Printf.printf "[EOF]\n"; exit 0) else
      Bytes.get bytes 0

  let put_char : char -> unit = fun c ->
    let open Printf in
    let n = Char.code c in
    let printable = n >= 32 && n <= 126 in
    let nl = (n == 10) in
    let bs = (n == 8) in
    if printable || nl || bs then printf "%c%!" c else printf "\\%02x%!" n

  type 'a ref  = 'a Stdlib.ref
  let ref = Stdlib.ref
  let (!) = (!)
  let (:=) = (:=)

  type bytes = Bytes.t

  let make_bytes n =
    (*Printf.printf "[Prim.make_bytes:%d]\n" n;*)
    (*Bytes.create n*)
    Bytes.make n '\000' (* bf specifies zero init *)

  let freeze_bytes = Bytes.(*unsafe_*)to_string
  let thaw_bytes = Bytes.(*unsafe_*)of_string
  let set_bytes = Bytes.set
  let get_bytes = Bytes.get

  let string_length = String.length
  let string_index s i = s.[i]

  let n_sectors = 32
  let sector_size = 512

  let with_open_disk f =
    let fd = Unix.openfile "disk.image" [O_RDWR;O_CREAT] 0o640 in
    Unix.ftruncate fd (sector_size * n_sectors);
    f fd;
    Unix.close fd

  let load_sector n buf =
    (*Printf.printf "[load_sector:%d]\n" n;*)
    if n < 0 then Printf.printf "[Prim.load_sector:%d]: too small\n" n else
      if n >= n_sectors then Printf.printf "[Prim.load_sector:%d]: too big\n" n else
        with_open_disk (fun fd ->
            let pos = n * sector_size in
            let (_:int) = Unix.lseek fd pos SEEK_SET in
            let (_:int) = Unix.read fd buf 0 sector_size in
            ())

  let store_sector n s =
    (*Printf.printf "[store_sector:%d]\n" n;*)
    if n < 0 then Printf.printf "[Prim.store_sector:%d]: too small\n" n else
      if n >= n_sectors then Printf.printf "[Prim.store_sector:%d]: too big\n" n else
        with_open_disk (fun fd ->
            let pos = n * sector_size in
            let (_:int) = Unix.lseek fd pos SEEK_SET in
            let (_:int) = Unix.write fd (Bytes.of_string s) 0 sector_size in
            ())

  let free_words () =
    0 (* return some dummy value *)

  let init_interrupt_mode () =
    ()

  let get_ticks () =
    0 (* return some dummy value *)

  let wait_for_interrupt : unit -> unit = fun () ->
    ()

  let is_keyboard_ready : unit -> bool = fun () ->
    true

  let normal_press : char -> int = fun c ->
    match c with
    | '1' -> 0x02
    | '2' -> 0x03
    | '3' -> 0x04
    | '4' -> 0x05
    | '5' -> 0x06
    | '6' -> 0x07
    | '7' -> 0x08
    | '8' -> 0x09
    | '9' -> 0x0a
    | '0' -> 0x0b
    | '-' -> 0x0c
    | '=' -> 0x0d
    | 'q' -> 0x10
    | 'w' -> 0x11
    | 'e' -> 0x12
    | 'r' -> 0x13
    | 't' -> 0x14
    | 'y' -> 0x15
    | 'u' -> 0x16
    | 'i' -> 0x17
    | 'o' -> 0x18
    | 'p' -> 0x19
    | '[' -> 0x1a
    | ']' -> 0x1b
    | 'a' -> 0x1e
    | 's' -> 0x1f
    | 'd' -> 0x20
    | 'f' -> 0x21
    | 'g' -> 0x22
    | 'h' -> 0x23
    | 'j' -> 0x24
    | 'k' -> 0x25
    | 'l' -> 0x26
    | ';' -> 0x27
    | '\'' -> 0x28
    | '`' -> 0x29
    | '\\' -> 0x2b
    | 'z' -> 0x2c
    | 'x' -> 0x2d
    | 'c' -> 0x2e
    | 'v' -> 0x2f
    | 'b' -> 0x30
    | 'n' -> 0x31
    | 'm' -> 0x32
    | ',' -> 0x33
    | '.' -> 0x34
    | '/' -> 0x35
    | ' ' -> 0x39
    | '\127' -> 0x0E (* from backspace key *)
    | '\t' -> 0x0F
    | '\n' -> 0x1C

    | _ -> 0


  let shifted_press : char -> int = fun c ->
    match c with
    | '!' -> 0x02
    | '@' -> 0x03
    | '#' -> 0x04
    | '$' -> 0x05
    | '%' -> 0x06
    | '^' -> 0x07
    | '&' -> 0x08
    | '*' -> 0x09
    | '(' -> 0x0A
    | ')' -> 0x0B
    | '_' -> 0x0C
    | '+' -> 0x0D
    | 'Q' -> 0x10
    | 'W' -> 0x11
    | 'E' -> 0x12
    | 'R' -> 0x13
    | 'T' -> 0x14
    | 'Y' -> 0x15
    | 'U' -> 0x16
    | 'I' -> 0x17
    | 'O' -> 0x18
    | 'P' -> 0x19
    | '{' -> 0x1A
    | '}' -> 0x1B
    | 'A' -> 0x1E
    | 'S' -> 0x1F
    | 'D' -> 0x20
    | 'F' -> 0x21
    | 'G' -> 0x22
    | 'H' -> 0x23
    | 'J' -> 0x24
    | 'K' -> 0x25
    | 'L' -> 0x26
    | ':' -> 0x27
    | '"' -> 0x28
    | '~' -> 0x29
    | '|' -> 0x2B
    | 'Z' -> 0x2C
    | 'X' -> 0x2D
    | 'C' -> 0x2E
    | 'V' -> 0x2F
    | 'B' -> 0x30
    | 'N' -> 0x31
    | 'M' -> 0x32
    | '<' -> 0x33
    | '>' -> 0x34
    | '?' -> 0x35

    | _ -> 0

  let press_shift = 42
  let release_shift = 170
  let press_control = 29
  let release_control = 157
  let dummy_release = 222

  let scan_code_list_of_ascii : char -> int list = fun c ->
    let n = ord c in
    if n <= 26 && c != '\n' then [ press_control; shifted_press (chr (n + ord '@')); dummy_release; release_control ] else
      let code = normal_press c in
      if code != 0 then [ code; dummy_release ] else
        let code = shifted_press c in
        if code != 0 then [ press_shift; code; dummy_release; release_shift ] else
          []

  let waiting : int list ref = ref []

  let rec get_keyboard_last_scancode : unit -> char = fun () ->
    match !waiting with
    | first::rest ->
       waiting := rest;
       chr first
    | [] ->
       let c = get_char() in (* this is blocking *)
       waiting := scan_code_list_of_ascii c;
       get_keyboard_last_scancode ()

end
include X
