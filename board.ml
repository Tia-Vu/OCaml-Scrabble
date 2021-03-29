exception IllegalMove

type tile = {
  letter : char;
  (* If no letter tile is placed, char is '.'*)
  (* -1, -1 if empty tile. Else, the coordinate of the letter tile*)
  coord : int * int;
}

type adjacent_tiles = {
  left : tile;
  up : tile;
  right : tile;
  down : tile;
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

let create_tile l x y = { letter = l; coord = (x, y) }

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

let empty_board j = failwith "unimplemented"

(*get_tile [coord] returns the tile at [coord] Requires: [coord] is in
  the form [row][col]*)
let get_tile coord tile_board =
  let row = fst coord in
  let col = snd coord in
  tile_board.(row).(col)

(*get_adacent_tiles [tile] returns the adjacent tiles starting with the
  tile to the left and going clockwise*)
let get_adjacent_tiles tile tile_board =
  let row = fst tile.coord in
  let col = snd tile.coord in
  {
    left = get_tile (row, col - 1) tile_board;
    up = get_tile (row + 1, col) tile_board;
    right = get_tile (row, col + 1) tile_board;
    down = get_tile (row - 1, col) tile_board;
  }

(* Show the current string in pretty print*)
let to_string = failwith "unimplemented"

(** Helper function to check if word is in dictionary*)
let word_in_dict word dict = failwith "unimplemented"

(** Helper function to check if the tile placement is near a current
    tile*)
let tiles_near_current_tiles = failwith "unimplemnted"

(** Use the two helper functions above to check if a placement is legal*)
let placement_is_legal t word start_coord direction = true

(*still unimplemented*)

(*place_tile [letter] [coord] [tile_board] places [letter] on the
  coordinate [coord] on [tile_board]. [coord] is in the order [row][col]

  Requires: is a valid placement*)
let place_tile letter coord tile_board =
  tile_board.(fst coord).(snd coord) <- { letter; coord }

(*to_letter_lst [word] returns [word] converted into a list of the
  letters in the list in the same order. Ex. to_letter_lst "hello"
  returns ['h';'e';'l';'l';'o']*)
let to_letter_lst word =
  let rec to_letter_lst_h word letter_lst =
    match word with
    | "" -> List.rev letter_lst
    | _ ->
        to_letter_lst_h
          (String.sub word 1 (String.length word - 1))
          (word.[0] :: letter_lst)
  in
  to_letter_lst_h word []

let rec place_tiles_hor letter_lst curr_coord tile_board =
  let next_coord = (fst curr_coord, snd curr_coord + 1) in
  match letter_lst with
  | [] -> tile_board
  | h :: t ->
      place_tile h curr_coord tile_board;
      place_tiles_hor t next_coord tile_board

let rec place_tiles_ver letter_lst curr_coord tile_board =
  let next_coord = (fst curr_coord - 1, snd curr_coord) in
  match letter_lst with
  | [] -> tile_board
  | h :: t ->
      place_tile h curr_coord tile_board;
      place_tiles_ver t next_coord tile_board

let place_tiles t word start_coord direction =
  match placement_is_legal t word start_coord direction with
  | true -> (
      match direction with
      | true ->
          {
            n = t.n;
            tile_board =
              place_tiles_hor (to_letter_lst word) start_coord
                t.tile_board;
            info_board = t.info_board;
          }
      | false ->
          {
            n = t.n;
            tile_board =
              place_tiles_ver (to_letter_lst word) start_coord
                t.tile_board;
            info_board = t.info_board;
          } )
  | false -> raise IllegalMove

(*Return a new board from json*)
let from_json json = failwith "unimplemented"

(* Score stuff *)
