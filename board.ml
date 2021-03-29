type tile = {
  letter : char;
  (* If no letter tile is placed, char is '.'*)
  (* -1, -1 if empty tile. Else, the coordinate of the letter tile*)
  coor : int * int;
  adjacent_tiles : tile list; (* Adjacent letter tiles - not empty ones*)
}

(*TODO: Bonus should be more sophisticated *)

(** [itile] is an information tile for the info_board. *)
type itile = { bonus : int }

type t = {
  n : int;
  (* Dimension of the board*)
  (* Take cares of tile placement on board n x n. tile_board[row][col]*)
  tile_board : tile array array;
  (*Take cares of board info n x n (double score) TODO: someday*)
  info_board : itile array array;
}

let create_tile l x y =
  { letter = l; coor = (x, y); adjacent_tiles = [] }

let init_tile () = create_tile '.' (-1) (-1)

let init_itile b = { bonus = b }

let init_tile_board n =
  let init_row n = Array.make n (init_tile ()) in
  Array.make n (init_row n)

let init_info_board n =
  let init_row n = Array.make n (init_itile 0) in
  Array.make n (init_row n)

let init_board n =
  { n; tile_board = init_tile_board n; info_board = init_info_board n }

let empty_board j = init_board 1 (*TODO: Placeholder*)

let place_tiles x = failwith "unimplemented"

let row_to_string row =
  let add_letter str t = str ^ " " ^ Char.escaped t.letter in
  let spaced_str = Array.fold_left add_letter "" row in
  String.sub spaced_str 1 (String.length spaced_str - 1)

(*Array.fold_left add_letter "" row*)

let to_string b =
  let rows = Array.map row_to_string b.tile_board in
  (*String.sub (Array.fold_left ( ^ ) "" rows) 1 (b.n - 1)*)
  let add_row str row = str ^ "\n" ^ row in
  let entered_str = Array.fold_left add_row "" rows in
  String.sub entered_str 1 (String.length entered_str - 1)

(** Helper function to check if word is in dictionary*)
let word_in_dict word dict = ()

(*failwith "unimplemnted"*)

(** Helper function to check if the tile placement is near a current
    tile*)
let tiles_near_current_tiles = ()

(*failwith "unimplemnted"*)

(** Use the two helper functions above to check if a placement is legal*)
let placement_is_legal t placement = ()

(*failwith "unimplemnted"*)

(*Return a new board from json*)
let from_json json = () (*failwith "unimplemented"*)

                        (* Score stuff *)
