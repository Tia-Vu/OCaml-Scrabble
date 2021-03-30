(** The module in charge of pretty printing the current board in the
    command line. *)

let print_board board = print_endline (Board.to_string board)

let print_scores scores = failwith "unimplemented"

let print_intro () = print_endline "\nThis is OCaml Scrabble!\n"

let print_end () = print_endline "Thank you for playing!"
