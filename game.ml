open Board
open Display
open Score

(** TODO: This is an incomplete implementation *)
type t = {
  board : Board.t;
  scores : Score.t;
      (* hand: ; scores:[0,1,2,] tilepool: shuffle (private), draw x
         tiles (public), is_emtpy, initialization *)
}

(** TODO: Change according to input of Board.place_tiles *)

(** GIBBERISH: erase when want to let play_turn prompt player_one_plays
    (something on somethingwhere)

    player_one_plays board.put something |> change_score |> *)
let game () = failwith "Unimplemented"

(* Execute the game engine. *)
let () = game ()
