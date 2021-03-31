open Yojson.Basic.Util

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
  dict : string list;
}

(*Return a new dictionary from json*)
let dict_from_json json = json |> to_assoc |> List.map (fun (x, y) -> x)

let create_tile l x y = { letter = l; coord = (x, y) }

let init_tile () = create_tile '.' (-1) (-1)

let init_itile b = { bonus = b }

let init_tile_board n =
  let init_row n i = Array.make n (init_tile ()) in
  Array.init n (init_row n)

let init_info_board n =
  let init_row n i = Array.make n (init_itile 0) in
  Array.init n (init_row n)

let empty_board json_dict n =
  {
    n;
    tile_board = init_tile_board n;
    info_board = init_info_board n;
    dict = dict_from_json json_dict;
  }

(*TODO: Make dictionary for board*)

(*get_tile [coord] returns the tile at [coord] Requires: [coord] is in
  the form [row][col]*)
let get_tile (row, col) tile_board = tile_board.(row).(col)

(*get_adacent_tiles [tile] returns the adjacent tiles starting with the
  tile to the left and going clockwise*)
let get_adjacent_tiles tile tile_board =
  let row = fst tile.coord in
  let col = snd tile.coord in
  {
    left = get_tile (row, col - 1) tile_board;
    up = get_tile (row - 1, col) tile_board;
    right = get_tile (row, col + 1) tile_board;
    down = get_tile (row + 1, col) tile_board;
  }

let row_to_string row =
  let add_letter str t = str ^ " " ^ Char.escaped t.letter in
  let spaced_str = Array.fold_left add_letter "" row in
  String.sub spaced_str 1 (String.length spaced_str - 1)

let to_string b =
  let rows = Array.map row_to_string b.tile_board in
  let add_row str row = str ^ "\n" ^ row in
  let entered_str = Array.fold_left add_row "" rows in
  String.sub entered_str 1 (String.length entered_str - 1)

(** Helper function to check if word is in dictionary*)
let word_in_dict dict word = List.mem word dict

(** Helper function to raise Error if word is not in dictionary*)
let check_in_dict dict word =
  if word_in_dict dict word then () else raise IllegalMove

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

(** [tile_occupied tle] checks is [tle] has a letter. *)
let tile_occupied tle = tle.letter <> '.'

(**[tiles_occupied_hor t w (row,col) length idx] is a helper function
   for tiles_occupied that checks horizontally Precondition: (row,col)
   must be a valid place on the board, letters is nonempty*)
let rec tiles_occupied_hor tile_board letters (row, col) =
  let board_letter = (get_tile (row, col) tile_board).letter in
  match letters with
  | [] -> false
  | h :: t ->
      if board_letter = '.' || h = board_letter then
        tiles_occupied_hor tile_board t (row, col + 1)
      else true

(**Same as tiles_occupied_hor but vertical*)
let rec tiles_occupied_ver tile_board letters (row, col) =
  let board_letter = (get_tile (row, col) tile_board).letter in
  match letters with
  | [] -> false
  | h :: t ->
      if board_letter = '.' || h = board_letter then
        tiles_occupied_ver tile_board t (row + 1, col)
      else true

(** [tiles_occupied t w start_coord dir] check if there are no tiles on
    the spots that [word] is expected to be placed on, or else they must
    be the same letter PLACEHOLDER*)
let tiles_occupied t w start_coord dir =
  if dir then
    tiles_occupied_hor t.tile_board (to_letter_lst w) start_coord
  else tiles_occupied_hor t.tile_board (to_letter_lst w) start_coord

(**Helper function to check if tile placement will be on the board*)
let off_board t word (row, col) direction =
  match direction with
  | true -> row + String.length word > t.n || row < 0
  | false -> col + String.length word > t.n || col < 0

(**[tiles_near_current_tile] gives whether the current tile at
   [(row,col)] has any tiles adjacent to it*)

let tiles_near_current_tile tile_board (row, col) =
  let adjacent =
    get_adjacent_tiles (get_tile (row, col) tile_board) tile_board
  in
  adjacent.left.letter <> '.'
  && adjacent.right.letter <> '.'
  && adjacent.up.letter <> '.'
  && adjacent.down.letter <> '.'

(**[tiles_near_current_tiles] t idx (row,col) dir] gives whether there
   are tiles adjacent to the tiles starting at the tile at [(row,col)]
   and going [idx] in the direction [dir] (horizontal if true and
   vertical if false)

   Precondition: there is a tile at [(row,col)] all the way to
   [(row,col)] + idx in the direction [dir]*)
let rec tiles_near_current_tiles t idx (row, col) dir =
  match idx with
  | 0 -> false
  | _ ->
      if tiles_near_current_tile t.tile_board (row, col) then true
      else if dir then
        tiles_near_current_tiles t (idx - 1) (row, col + 1) dir
      else tiles_near_current_tiles t (idx - 1) (row + 1, col) dir

(* [word_start_hor t start_coord] is the starting x coordinate of the
   horizontal word that is a superset of the tile on [start_coord]*)
let word_start_hor t start_coord =
  let x0, y = start_coord in
  let x = ref x0 in
  let b = t.tile_board in
  let _ =
    while b.(!x).(y) |> tile_occupied do
      x := !x - 1
    done
  in
  !x + 1

(** [horizontal_word_of t (x,y)] gives the maximum horizontal superset
    word that consists of the letter at [(x,y)] on [t]. Example: If
    [(x,y)] is at 'a' for ". . . p i n e a p p l e . ." , it returns
    "pineapple" PLACHOLDER *)
let horizontal_word_of t start_coord =
  let word = ref "" in
  let _ =
    let x = ref (word_start_hor t start_coord) in
    let _, y = start_coord in
    let b = t.tile_board in
    while b.(!x).(y) |> tile_occupied do
      word := !word ^ Char.escaped b.(!x).(y).letter;
      x := !x + 1
    done
  in
  !word

(* [word_start_ver t start_coord] is the starting y coordinate of the
   vertical word that is a superset of the tile on [start_coord]*)
let word_start_ver t start_coord =
  let x, y0 = start_coord in
  let y = ref y0 in
  let b = t.tile_board in
  let _ =
    while b.(x).(!y) |> tile_occupied do
      y := !y - 1
    done
  in
  !y + 1

(** [vertical_word_of t (x,y)] gives the maximum vertical superset word
    that consists of the letter at [(x,y)] on [t]. Similar to
    [horizontal_word_of t] but for vertical words *)
let vertical_word_of t start_coord =
  let word = ref "" in
  let _ =
    let y = ref (word_start_ver t start_coord) in
    let x, _ = start_coord in
    let b = t.tile_board in
    while b.(x).(!y) |> tile_occupied do
      word := !word ^ Char.escaped b.(x).(!y).letter;
      y := !y + 1
    done
  in
  !word

(*place_tile [letter] [coord] [tile_board] places [letter] on the
  coordinate [coord] on [tile_board]. [coord] is in the order [row][col]

  Requires: is a valid placement*)
let place_tile letter coord tile_board =
  tile_board.(fst coord).(snd coord) <- { letter; coord }

let rec place_word_hor letter_lst curr_coord tile_board =
  let next_coord = (fst curr_coord, snd curr_coord + 1) in
  match letter_lst with
  | [] -> tile_board
  | h :: t ->
      place_tile h curr_coord tile_board;
      place_word_hor t next_coord tile_board

let rec place_word_ver letter_lst curr_coord tile_board =
  let next_coord = (fst curr_coord + 1, snd curr_coord) in
  match letter_lst with
  | [] -> tile_board
  | h :: t ->
      place_tile h curr_coord tile_board;
      place_word_ver t next_coord tile_board

(** [place_word_no_validation t w (x,y) dir] places word without
    validation check*)
let place_word_no_validation t word start_coord dir =
  match dir with
  | true ->
      {
        t with
        n = t.n;
        tile_board =
          place_word_hor (to_letter_lst word) start_coord t.tile_board;
        info_board = t.info_board;
      }
  | false ->
      {
        t with
        n = t.n;
        tile_board =
          place_word_ver (to_letter_lst word) start_coord t.tile_board;
        info_board = t.info_board;
      }

(** Check if a placement is legal for a horizontally placed word. *)
let placement_is_legal_hor t word start_coord =
  let expected_t = place_word_no_validation t word start_coord true in
  let check_horizontal_is_valid_word =
    horizontal_word_of expected_t start_coord |> check_in_dict t.dict
  in
  let x0, y0 = start_coord in
  let l = String.length word in
  let check_vertical_for_each_letter =
    for x = x0 to x0 + l do
      vertical_word_of expected_t (x, y0) |> check_in_dict t.dict
    done
  in
  true

(** Check if a placement is legal for a vertically placed word. *)
let placement_is_legal_ver t word start_coord =
  let expected_t = place_word_no_validation t word start_coord false in
  let check_vertical_is_valid_word =
    vertical_word_of expected_t start_coord |> check_in_dict t.dict
  in
  let x0, y0 = start_coord in
  let l = String.length word in
  let check_horizontal_for_each_letter =
    for y = y0 to y0 + l do
      horizontal_word_of expected_t (x0, y) |> check_in_dict t.dict
    done
  in
  true

(** Check if a placement is legal*)
let placement_is_legal t word start_coord direction =
  if
    off_board t word start_coord direction
    || tiles_occupied t word start_coord direction
    || not
         (tiles_near_current_tiles t (String.length word) start_coord
            direction)
  then false
  else if direction then placement_is_legal_hor t word start_coord
  else placement_is_legal_ver t word start_coord

(*still unimplemented*)

let place_word t word start_coord direction =
  match placement_is_legal t word start_coord direction with
  | true -> place_word_no_validation t word start_coord direction
  | false -> raise IllegalMove

(* Score stuff *)
