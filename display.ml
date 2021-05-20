(** The module in charge of pretty printing the current board in the
    command line. *)

let print_board board = print_endline ("\n" ^ Board.to_string board)

let print_scores scores =
  print_endline ("\nYour Current Score: " ^ Score.to_string scores)

let print_hand hand =
  print_endline ("\nYour Hand: " ^ Hand.to_string hand)

(** [print_separator ()] prints a line of dashes that separates a
    display*)
let print_separator () =
  print_endline "\n------------------------------------------"

let print_try_again () = print_string "\nPlease try again.\n"

let print_round_start () = print_string "\n A new round has begun.\n"

let print_round_end () = print_string "\n The round has ended. \n"

let print_player_turn n =
  print_string ("\n Player " ^ string_of_int n ^ "'s turn!")

let print_exc_board_illegal_move s =
  print_endline ("\nThis is an illegal move. " ^ s)

let print_exc_malformed () =
  print_endline "\nThis is not a valid command"

let print_exc_hand_insufficient_tiles () =
  print_endline
    "\nThe hand does not have enough letter tiles to place the word."

let print_move_instructions () =
  print_separator ();
  print_endline "Make your move!";
  print_endline
    "\n\
     You may either enter [Draw] to discard your current hand for a \
     new one, or";
  print_endline
    "enter a move to place on the board in the form of [word row col \
     direction] with";
  print_endline "direction being [hor] or [ver].";
  print_string ">"

let print_intro () = print_endline "\nThis is OCaml Scrabble!\n"

let print_end () = print_endline "\nThank you for playing!\n"

let print_pool p =
  print_endline ("Letter Pool: " ^ Pool.to_string p ^ "tiles left.")
