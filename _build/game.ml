open Board
open Display
open Score

(** TODO: This is an incomplete implementation *)
type game_state = {
  board : Board.t;
  scores : Score.t;
      (* hand: ; scores:[0,1,2,] tilepool: shuffle (private), draw x
         tiles (public), is_emtpy, initialization *)
}

(** TODO: Change according to input of Board.place_tiles *)

(** GIBBERISH: erase when want to let play_turn prompt player_one_plays
    (something on somethingwhere)

    player_one_plays board.put something |> change_score |> *)

let end_not_reached game_state = true

let intake_move () = failwith "Unimplemented"

let update_game_state s move = failwith "Unimplemented"

let play_game s =
  let rec pass_turns state continue =
    match continue with
    | true ->
        let new_state = update_game_state state (intake_move ()) in
        print_board new_state.board;
        print_scores new_state.scores;
        pass_turns new_state (end_not_reached new_state)
    | false -> print_endline "No more moves can be made"
  in
  pass_turns s true

let rec dict_prompt () =
  print_endline
    "Please enter the (valid) name of the json dictionary file you \
     want to load.\n";
  print_string "> ";
  let file_name = read_line () in
  if Sys.file_exists file_name then Yojson.Basic.from_file file_name
  else dict_prompt ()

let game () =
  print_intro ();
  let new_board = empty_board (dict_prompt ()) in
  let new_scores = create () in
  play_game { board = new_board; scores = new_scores };
  print_end ();
  exit 0

(* Execute the game engine. *)
let () = game ()
