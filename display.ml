(** The module in charge of pretty printing the current board in the
    command line. *)

let print_scores scores =
  print_endline ("\nYour Current Score: " ^ Score.to_string scores)

let print_hand hand =
  print_endline ("\n\nYour Hand: " ^ Hand.to_string hand)

(** [print_separator ()] prints a line of dashes that separates a
    display*)
let print_separator () =
  print_endline "\n------------------------------------------"

let print_try_again () = print_string "\nPlease try again.\n"

let print_round_start () =
  print_separator ();
  print_string "\n A new round has begun.\n"

let print_round_end () =
  print_separator ();
  print_string "\nThe round has ended. \n";
  print_separator ()

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
  print_endline "\nMake your move!";
  print_endline
    "\n\
     You may either enter [Draw] to discard your current hand for a \
     new one,";
  print_endline
    "enter a move to place on the board in the form of [word row col \
     direction] with";
  print_endline
    "direction being [hor] or [ver], or [Quit] to leave the game.";
  print_endline
    "Scores of quitted players will still be considered in final \
     rankings.";
  print_endline "The game will end if all players have [Quit].";
  print_string ">"

let print_intro () = print_endline "\nThis is OCaml Scrabble!\n"

let print_end () = print_endline "\nThank you for playing!\n"

let print_winner score = function
  | [ n ] ->
      print_endline
        ( "\nThe winner is player " ^ string_of_int n ^ " with score "
        ^ string_of_int score ^ "!" )
  | [ fst; snd ] ->
      print_endline
        ( "\nThe winners are players " ^ string_of_int fst ^ " and "
        ^ string_of_int snd ^ " with score " ^ string_of_int score ^ "!"
        )
  | lst ->
      print_endline
        ( "\nThe winners are players "
        ^ List.fold_left
            (fun acc x -> acc ^ string_of_int x ^ ", ")
            "" lst
        ^ " with score " ^ string_of_int score ^ "!" )

let print_pool p =
  print_endline ("\nLetter Pool: " ^ Pool.to_string p ^ " tiles left.")

(*let print_board board = print_endline ("\n" ^ Board.to_string board)*)
let print_board board = Board.print_board board
