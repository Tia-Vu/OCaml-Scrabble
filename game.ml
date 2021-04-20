open Board
open Hand
open Display
open Score
open Pool
open Yojson.Basic.Util

(** TODO: This is an incomplete implementation. Will contain more fiels
    like scores = Score.t once Score is implemented.*)
type game_state = {
  board : Board.t;
  hand : Hand.t;
  (* Things to potentially add: hand: ; scores:[0,1,2,] tilepool:
     shuffle (private), draw x tiles (public), is_emtpy, initialization *)
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

let rec parse_place_word (s : string) : place_word_command =
  let parsed = String.split_on_char ' ' s in
  match parsed with
  | [ w; x; y; dir ] ->
      {
        word = w;
        start_coord = (int_of_string x, int_of_string y);
        direction =
          (if dir = "hor" then true
          else if dir = "ver" then false
          else raise Malformed
            (*TODO if dir is anything else than "hor" or "ver" then it
              fails*));
      }
  | _ -> raise Malformed

(**[update_game_state] is the new game state after the board, hand, and
   score in old state [s] is updated using the passed in move[input].*)
let update_game_state s input =
  match input with
  | "Draw" ->
      let redrawn_hand = draw_nletters s.pool 10 (empty_hand ()) in
      { s with board = s.board; hand = redrawn_hand }
  | _ -> (
      match parse_place_word input with
      | exception Malformed -> raise Malformed
      | cmd ->
          let placed =
            place_word s.board cmd.word cmd.start_coord cmd.direction
          in
          let new_hand =
            s.hand |> spend_word cmd.word |> fill_hand s.pool 7
          in
          (*OLD: Later when we have scores, update this part of the
            record*)
          { s with board = placed; hand = new_hand })

(** [play_game] runs each turn. If the game should not terminate yet, it
    will prompt for user input and update the game state accordingly. If
    the user input is not a valid move, it will prompt for another move.
    If the input is valid, the game state will change, and the new board
    and hand (and score, when implemented) will be printed.*)
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
            print_hand new_state.hand;
            pass_turns new_state (continue_game new_state))
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
  let new_pool = init_pool () in
  (*TODO: Replace 6 with user input*)
  play_game
    {
      board = new_board;
      (*TODO: replace 7 tiles with something else?*)
      hand = fill_hand new_pool 7 (empty_hand ());
      letter_points = read_lpts "letter_points.json";
      pool = new_pool;
    };

  print_end ();
  exit 0

(* Execute the game engine. *)
let () = run ()
