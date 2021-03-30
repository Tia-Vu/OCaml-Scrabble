open Board
open Display
open Score

(** TODO: This is an incomplete implementation *)
type game_state = {
  board : Board.t;
  scores : Score.t;
      (* Things to potentially add: hand: ; scores:[0,1,2,] tilepool:
         shuffle (private), draw x tiles (public), is_emtpy,
         initialization *)
}

type place_word_command = {
  word : string;
  start_coord : int * int;
  direction : bool;
}

(** Checks if there are any possible moves. [continue_game] is true if
    there are still possible moves to be made, and false if not.*)
let continue_game game_state = false

(**[input_move] prompts the player for a move and returns it.*)
let intake_move () =
  print_endline "Make your move:";
  print_string "> ";
  read_line ()

let parse_place_word (s : string) : place_word_command =
  let parsed = String.split_on_char ' ' s in
  match parsed with
  | w :: x :: y :: dir :: t ->
      {
        word = w;
        start_coord = (int_of_string x, int_of_string y);
        direction =
          ( if dir = "hor" then true
          else false
            (*TODO if dir is anything else than "hor" then its false*)
          );
      }
  | _ -> failwith "Wrong"

(**[update_game_state] is the new game state after the board and score
   in old state [s] is updated using the passed in [move].*)
let update_game_state s move =
  let placed = place_tiles s.board move in
  { board = fst placed; scores = update_score s.scores (snd placed) }

(** [play_game] runs each turn, updating the game state, printing the
    new board and score, and checks if the game should terminate.*)
let play_game s =
  let rec pass_turns state continue =
    match continue with
    | true ->
        let new_state = update_game_state state (intake_move ()) in
        print_board new_state.board;
        print_scores new_state.scores;
        pass_turns new_state (continue_game new_state)
    | false -> print_endline "No more moves can be made"
  in
  pass_turns s true

(**[dict_prompt] prompts the player for the name of a json file of the
   dictionary to be used for the current game and returns Yojson.Basic.t
   of that json file.*)
let rec dict_prompt () =
  print_endline
    "Please enter the (valid) name of the json dictionary file you \
     want to load.\n";
  print_string "> ";
  let file_name = read_line () in
  if Sys.file_exists file_name then Yojson.Basic.from_file file_name
  else dict_prompt ()

(**[game] is the main method of the Scrabble game. It initializes an
   empty board and score and starts the passing of turns, eventually
   terminating the game when the turns are done.*)
let game () =
  print_intro ();
  let new_board = empty_board (dict_prompt ()) in
  let new_scores = create () in
  play_game { board = new_board; scores = new_scores };
  print_end ();
  exit 0

(* Execute the game engine. *)
let () = game ()
