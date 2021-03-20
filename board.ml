type tile ={
  letter : char; (* If no letter tile is placed, char is '.'*)
  coor : int * int;
  adjacent_tiles : tile list;
}
(*-1 -1 -1 -1 -1 
  -1 -1 L -1  -1
  -1 -1 E -1  -1
  -1 -1 A -1  -1
  -1 -1 F -1  -1*)
 (* The grids implement by 2 dimensional array*)
type t = {
  n:int (* Dimension of the board*);
  (* n x n array of tile*)
  tile_board = (* Take cares of tile placement on board n x n*);
  info_board = (*Take cares of board info n x n (double score) TODO: someday*);
}

(*
1) implementation of type t 1.5) to_string
2) test 
*)
let place_tiles = failwith "unimplemented"

(* Show the current string in pretty print*)
let to_string = failwith "unimplemented"

(** Helper function to check if word is in dictionary*)
let word_in_dict word dict = failwith "unimplemnted"

(** Helper function to check if the tile placement is near a current tile*)
let tiles_near_current_tiles = failwith "unimplemnted" 

(** Use the two helper functions above to check if a placement is legal*)
let placement_is_legal t placement =failwith "unimplemnted" 

(*Return a new board from json*)
let from_json json = failwith "unimplemented"
(* Score stuff *)
