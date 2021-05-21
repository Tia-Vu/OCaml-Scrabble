open Board
open Hand
open Display
open Score
open Pool
open Yojson.Basic.Util

type game_state = {
  board : Board.t;
  players : (int * Hand.t * Score.t) list;
  letter_points : (char * int) list;
  pool : Pool.t;
}

type place_word_command = {
  word : string;
  start_coord : int * int;
  direction : bool;
}

(* Checks if there are any possible moves. [continue_game] is true if
   there are still possible moves to be made, and false if not.*)
let continue_game game_state = true

(*[read_input_move] prompts the player for a move and returns it.*)
let read_input_move () =
  print_move_instructions ();
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
          ( if dir = "hor" then true
          else if dir = "ver" then false
          else raise Malformed
            (*TODO if dir is anything else than "hor" or "ver" then it
              fails*) );
      }
  | _ -> raise Malformed

(*[update_player_state] returns a new game state with the tuple of
  (player number, hand, score) that has been updated using the passed in
  move[input] being added to the player list.*)
let update_player_state s (n, hand, score) input =
  match input with
  | "Draw" ->
      print_endline "\nYou discard your hand and redraw.";
      let redrawn_hand = draw_nletters s.pool 7 (empty_hand ()) in
      { s with players = (n, redrawn_hand, score) :: s.players }
  | _ -> (
      match parse_place_word input with
      | exception Malformed -> raise Malformed
      | cmd ->
          let req_letters =
            requires_letters s.board cmd.word cmd.start_coord
              cmd.direction
          in
          let placed =
            place_word s.board cmd.word cmd.start_coord cmd.direction
          in
          let formed_words =
            get_created_words s.board cmd.word cmd.start_coord
              cmd.direction
          in
          let new_hand =
            hand |> spend_word req_letters |> fill_hand s.pool 7
          in
          let new_score = update_score score formed_words in
          print_endline ("\nYou place the word " ^ cmd.word ^ ".");
          {
            s with
            board = placed;
            players = (n, new_hand, new_score) :: s.players;
          } )

(*Prompts the player until a legal move has been played and returns a
  new game state with the player's move reflected.*)
let rec player_turn state (n, hand, score) =
  match
    update_player_state state (n, hand, score) (read_input_move ())
  with
  | exception Board.IllegalMove s ->
      print_exc_board_illegal_move s;
      print_try_again ();
      player_turn state (n, hand, score)
  | exception Malformed ->
      print_exc_malformed ();
      print_try_again ();
      player_turn state (n, hand, score)
  | exception Hand.InsufficentTiles ->
      print_exc_hand_insufficient_tiles ();
      print_try_again ();
      player_turn state (n, hand, score)
  | new_state -> new_state

(*Goes through the list of players, asking each for their moves and
  returning the final game state after all players have gone. The player
  list field in the state variable holds only the players whose turns
  have been processed. i.e. The "player" list should be empty when you
  pass in the state the first time. The actual list of players is the
  second argument that is being pattern matched.*)
let rec cycle_players state = function
  | [] -> state
  | (n, hand, score) :: t ->
      print_separator ();
      print_player_turn n;
      print_board state.board;
      print_hand hand;
      print_scores score;
      print_separator ();
      cycle_players
        (*state with players = player_turn state (n, hand, score) ::
          state.players;*)
        (player_turn state (n, hand, score))
        t

(* [play_game] runs each round. If the game should not terminate yet, it
   will prompt for user input and update the game state accordingly. The
   new board, hand, and score will be printed.*)
let play_game s =
  let rec pass_rounds state continue =
    match continue with
    | true ->
        print_round_start ();
        let new_state =
          cycle_players { state with players = [] } state.players
        in
        let new_state_rev_players =
          { new_state with players = List.rev new_state.players }
        in
        print_round_end ();
        pass_rounds new_state_rev_players
          (continue_game new_state_rev_players)
    | false -> print_endline "No more moves can be made."
  in
  pass_rounds s true

(*[dict_prompt] prompts the player for the name of a json file of the
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

let rec player_prompt () =
  print_endline
    "\nPlease enter the number of players in this Scrabble game.\n";
  print_string "> ";
  let number = read_line () in
  try
    let n = int_of_string number in
    if n >= 1 then n
    else (
      print_endline
        "\nPlease enter a number of players of at least 1. \n";
      player_prompt () )
  with _ ->
    print_endline "\nPlease enter a valid number. \n";
    player_prompt ()

(* [read_lpts fname] gives [letter_points] compatible data from json
   file with name [fname]*)
let read_lpts file_name =
  Yojson.Basic.from_file file_name
  |> to_assoc
  |> List.map (fun (x, y) -> (x.[0], to_int y))

(*Builds a list of as many players as is passed in, each with their
  player number and a new hand and score. Precondition: passed in number
  is >= 1.*)
let rec build_init_players pool acc = function
  | 0 -> acc
  | n ->
      build_init_players pool
        ((n, fill_hand pool 7 (empty_hand ()), create ()) :: acc)
        (n - 1)

(**[run] is the main method of the Scrabble game. It initializes an
   empty board and score and starts the passing of turns, eventually
   terminating the game when the turns are done.*)
let run () =
  print_intro ();
  let new_board = empty_board (dict_prompt ()) 25 in
  let new_pool = init_pool () in
  let player_number = player_prompt () in
  (*TODO: Replace 6 with user input*)
  let init_state =
    {
      board = new_board;
      players = build_init_players new_pool [] player_number;
      letter_points = read_lpts "letter_points.json";
      pool = new_pool;
    }
  in
  play_game init_state;

  print_end ();
  exit 0

(* Execute the game engine. *)
let () = run ()
