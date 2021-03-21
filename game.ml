open Board
open Display
open Score

(** TODO: This is an incomplete implementation *)
type dictionary = string list
(** TODO: This is an incomplete implementation *)
type t = { board: Board.t; dict: dictionary}

(** TODO: Change according to input of Board.place_tiles *)
let place_tiles = Board.place_tiles t.board ()

let game () = failwith "Unimplemented"

(* Execute the game engine. *)
let () = game ()
