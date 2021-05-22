open Board
open Hand
open Display
open Score
open Pool
open Yojson.Basic.Util

type game_state = {
  board : Board.t;
  (*Each player holds value true if they are still playing, false if
    they are not.*)
  players : (int * Hand.t * Score.t * bool) list;
  letter_points : (char * int) list;
  pool : Pool.t;
}

type place_word_command = {
  word : string;
  start_coord : int * int;
  direction : bool;
}

(* Checks if the game should end. [continue_game] is true if there are
   still players who have not quit, and false if not.*)
let continue_game players =
  List.fold_left
    (fun acc (_, _, _, playing) -> acc || playing)
    false players

(*[read_input_move] prompts the player for a move and returns it.*)
let read_input_move () =
  print_move_instructions ();
  read_line ()

exception Malformed

let int_of_string_raise_malformed i =
  match int_of_string i with
  | exception Failure _ -> raise Malformed
  | s -> s

let rec parse_place_word (s : string) : place_word_command =
  let parsed = String.split_on_char ' ' s in
  match parsed with
  | [ w; x; y; dir ] ->
      {
        word = w;
        start_coord =
          ( int_of_string_raise_malformed x,
            int_of_string_raise_malformed y );
        direction =
          ( dir = "hor"
          || if dir <> "ver" then raise Malformed else false );
      }
  | _ -> raise Malformed

let process_cmd s (n, hand, score, playing) cmd =
  let req_letters =
    requires_letters s.board cmd.word cmd.start_coord cmd.direction
  in
  let placed =
    place_word s.board cmd.word cmd.start_coord cmd.direction
  in
  let formed_words =
    get_created_words s.board cmd.word cmd.start_coord cmd.direction
  in
  let new_hand = hand |> spend_word req_letters |> fill_hand s.pool 7 in
  let new_score = update_score score formed_words in
  print_endline ("\nYou place the word " ^ cmd.word ^ ".");
  {
    s with
    board = placed;
    players = (n, new_hand, new_score, playing) :: s.players;
  }

(*[update_player_state] returns a new game state with the tuple of
  (player number, hand, score) that has been updated using the passed in
  move[input] being added to the player list.*)
let update_player_state s (n, hand, score, playing) input =
  match input with
  | "Quit" ->
      print_endline
        ("\nPlayer " ^ string_of_int n ^ " has quit the game.");
      { s with players = (n, hand, score, false) :: s.players }
  | "Draw" ->
      print_endline "\nYou discard your hand and redraw.";
      let redrawn_hand = draw_nletters s.pool 7 (empty_hand ()) in
      {
        s with
        players = (n, redrawn_hand, score, playing) :: s.players;
      }
  | _ -> (
      match parse_place_word input with
      | exception Malformed -> raise Malformed
      | cmd -> process_cmd s (n, hand, score, playing) cmd )

(*Prompts the player until a legal move has been played and returns a
  new game state with the player's move reflected.*)
let rec player_turn state (n, hand, score, playing) =
  match
    update_player_state state
      (n, hand, score, playing)
      (read_input_move ())
  with
  | exception Board.IllegalMove s ->
      print_exc_board_illegal_move s;
      print_try_again ();
      player_turn state (n, hand, score, playing)
  | exception Malformed ->
      print_exc_malformed ();
      print_try_again ();
      player_turn state (n, hand, score, playing)
  | exception Hand.InsufficentTiles ->
      print_exc_hand_insufficient_tiles ();
      print_try_again ();
      player_turn state (n, hand, score, playing)
  | new_state -> new_state

(*Goes through the list of players, asking each for their moves and
  returning the final game state after all players have gone. The player
  list field in the state variable holds only the players whose turns
  have been processed. i.e. The "player" list should be empty when you
  pass in the state the first time. The actual list of players is the
  second argument that is being pattern matched.*)
let rec cycle_players state = function
  | [] -> state
  | (n, hand, score, playing) :: t ->
      if playing then (
        print_separator ();
        print_player_turn n;
        print_board state.board;
        print_hand hand;
        print_scores score;
        cycle_players (player_turn state (n, hand, score, playing)) t )
      else
        cycle_players
          {
            state with
            players = (n, hand, score, playing) :: state.players;
          }
          t

(* [play_game] runs each round and is the final state after the game has
   ended. If the game should not terminate yet, it will prompt for user
   input and update the game state accordingly. The new board, hand, and
   score will be printed.*)
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
        print_pool new_state.pool;
        print_round_end ();
        pass_rounds new_state_rev_players
          (continue_game new_state_rev_players.players)
    | false ->
        print_endline "\nAll players have quit.";
        state
  in
  pass_rounds s true

(*[dict_prompt] prompts the player for the name of a json file of the
  dictionary to be used for the current game and returns Yojson.Basic.t
  of that json file.*)
let rec dict_prompt () =
  print_endline
    "\n\
     Please enter the (valid) name of the json dictionary file you \
     want to load or type [Default] to use the built in dictionary.\n";
  print_string "> ";
  let file_name = read_line () in
  if Sys.file_exists file_name then Yojson.Basic.from_file file_name
  else if file_name = "Default" then
    Yojson.Basic.from_file "dictionary.json"
  else dict_prompt ()

(*[bonus_prompt] prompts the player for the name of a json file of the
  dictionary to be used for the current game and returns Yojson.Basic.t
  option of that json file.*)
let rec bonus_prompt () =
  print_endline
    "\n\
     Please enter the (valid) name of the json bonus words file you \
     want to load.  Type [None] if you would not like any bonus words\n\
     or type [Default] to use the default Camel related bonus words.\n";
  print_string "> ";
  let file_name = read_line () in
  if Sys.file_exists file_name then
    Some (Yojson.Basic.from_file file_name)
  else if file_name = "None" then None
  else if file_name = "Default" then
    Some (Yojson.Basic.from_file "camel_bonus.json")
  else bonus_prompt ()

(*[size_prompt] prompts the player for the size of board they would like
  to use*)
let rec size_prompt () =
  print_endline
    "\n\
     Please enter the board size you would like to use. Typing 7 \
     results in a 7x7 board.  Valid board sizes are from 5 to 30. \n";
  print_string "> ";
  let size = read_line () in
  try
    let n = int_of_string size in
    if n >= 5 && n <= 30 then n
    else (
      print_endline "\nPlease enter a valid board size. \n";
      size_prompt () )
  with _ ->
    print_endline "\nPlease enter a valid number. \n";
    size_prompt ()

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
let rec build_init_players bonus_words pool acc = function
  | 0 -> acc
  | n ->
      build_init_players bonus_words pool
        ( (n, fill_hand pool 7 (empty_hand ()), create bonus_words, true)
        :: acc )
        (n - 1)

let build_number_score_list players =
  List.map (fun (n, _, s, _) -> (n, get_score s)) players

(*[compare_label_score (l1,s1) (l2,s2)] is positive if s1 < s2, negative
  if s1 > s2, and 0 if s1 = s2.*)
let compare_label_score (l1, s1) (l2, s2) = s2 - s1

(*[highest_score_label ls_list] is the number label (in list form) and
  corresponding score of the player with the highest score in ls_list, a
  list of labels and scores of form [(l1,s1);...(ln,sn)]. If multiple
  players have the same highest score, all number labels are returned in
  a list with that score in the form of ([l1,l2,..], s1).

  Precondition: The list of labels and scores is nonempty.*)
let highest_label_score (ls_list : (int * int) list) =
  let sorted = List.sort compare_label_score ls_list in
  match sorted with
  | (_, high_score) :: t ->
      ( List.fold_right
          (fun (n, s) acc -> if s = high_score then n :: acc else acc)
          sorted [],
        high_score )
  | [] -> failwith "Precondition failed"

let declare_winner players =
  let winners, score =
    highest_label_score (build_number_score_list players)
  in
  print_winner score winners

(**[run] is the main method of the Scrabble game. It initializes an
   empty board and score and starts the passing of turns, eventually
   terminating the game when the turns are done.*)
let run () =
  print_intro ();
  let size = size_prompt () + 1 in
  let dictionary = dict_prompt () in
  let bonus_words = bonus_prompt () in
  let new_board = empty_board dictionary bonus_words size in
  let new_pool = init_pool () in
  let player_number = player_prompt () in
  let init_state =
    {
      board = new_board;
      players = build_init_players bonus_words new_pool [] player_number;
      letter_points = read_lpts "letter_points.json";
      pool = new_pool;
    }
  in
  let final_state = play_game init_state in
  declare_winner final_state.players;
  print_end ();
  exit 0

(* Execute the game engine. *)
let () = run ()
