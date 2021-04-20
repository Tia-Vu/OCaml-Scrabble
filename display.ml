(** The module in charge of pretty printing the current board in the
    command line. *)

let print_board board = print_endline ("\n" ^ Board.to_string board)

let print_scores scores = print_endline (Score.to_string scores)

let print_hand hand = failwith "Unimplemented"

let print_intro () = print_endline "\nThis is OCaml Scrabble!\n"

let print_end () = print_endline "\nThank you for playing!\n"
