open Board
open Display
open Score
open Pool
open Yojson.Basic.Util

(** TODO: This is an incomplete implementation. Will contain more fiels
    like scores = Score.t once Score is implemented.*)
type game_state = {
  board : Board.t;
  (* Things to potentially add: hand: ; scores:[0,1,2,] tilepool:
     shuffle (private), draw x tiles (public), is_emtpy, initialization *)
  (* Associated list to lookup point for letter*)
  letter_points : (char * int) list;
  (* Letter pool of the game*)
  pool : Pool.t;
}

type place_word_command = {
  word : string;
  start_coord : int * int;
  direction : bool;
}

(** Checks if there are any possible moves. [continue_game] is true if
    there are still possible moves to be made, and false if not.*)
let continue_game game_state = true

(**[read_input_move] prompts the player for a move and returns it.*)
let read_input_move () =
  print_endline "\nMake your move:";
  print_string "> ";
  read_line ()

exception Malformed

let parse_place_word (s : string) : place_word_command =
  let parsed = String.split_on_char ' ' s in
  match parsed with
  | [ w; x; y; dir ] ->
      {
        word = w;
        start_coord = (int_of_string x, int_of_string y);
        direction =
          ( if dir = "hor" then true
          else if dir = "ver" then false
          else raise Malformed
            (*TODO if dir is anything else than "hor" or "ver" then it
              fails*) );
      }
  | _ -> raise Malformed

(**[input_move] prompts the player for a move in the form of "word x y
   direction" and returns it.*)
let input_move () =
  print_endline "Make your move:";
  print_string "> ";
  parse_place_word (read_line ())

(**[update_game_state] is the new game state after the board and score
   in old state [s] is updated using the passed in [move].*)
let update_game_state s input =
  match parse_place_word input with
  | exception Malformed -> raise Malformed
  | cmd ->
      let placed =
        place_word s.board cmd.word cmd.start_coord cmd.direction
      in
      (*OLD: Later when we have scores { board = fst placed; scores =
        update_score s.scores (snd placed) }*)
      { s with board = placed }

(** [play_game] runs each turn, updating the game state, printing the
    new board (and score, when implemented), and checks if the game
    should terminate.*)
let play_game s =
  let rec pass_turns state continue =
    match continue with
    | true -> (
        match update_game_state state (read_input_move ()) with
        | exception Board.IllegalMove s ->
            print_endline ("\nThis is an illegal move. " ^ s);
            print_string "\nPlease try again.\n";
            pass_turns state (continue_game state)
        | exception Malformed ->
            print_endline "\nThis is not a valid command";
            print_string "\nPlease try again.\n";
            pass_turns state (continue_game state)
        | new_state ->
            print_board new_state.board;
            pass_turns new_state (continue_game new_state) )
    | false -> print_endline "No more moves can be made."
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

(** [read_lpts fname] gives [letter_points] compatible data from json
    file with name [fname]*)
let read_lpts file_name =
  Yojson.Basic.from_file file_name
  |> to_assoc
  |> List.map (fun (x, y) -> (x.[0], to_int y))

(**[run] is the main method of the Scrabble game. It initializes an
   empty board and score and starts the passing of turns, eventually
   terminating the game when the turns are done.*)
let run () =
  print_intro ();
  let new_board = empty_board (dict_prompt ()) 25 in
  (*TODO: Replace 6 with user input*)
  play_game
    {
      board = new_board;
      letter_points = read_lpts "letter_points.json";
      pool = Pool.init_pool ();
    };

  print_end ();
  exit 0

(* Execute the game engine. *)
let () = run ()
