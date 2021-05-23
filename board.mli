(** Representation of a scrabble board.

    This module represents the scrabble board data, including its shape,
    location of bonus tiles, and its current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(**The bonus tile types [N] is None [DL] is Double Letter [TL] is Triple
   Letter [DW] is Double Word and [TW] is Triple Word.*)
type bonus =
  | N
  | DL
  | TL
  | DW
  | TW

(** The Exception [IllegalMove] is raised when an illegal move is
    attempted. *)
exception IllegalMove of string

(** [empty_board j b n] initializes an empty board of size [n * n] with
    the dictionary json [j] after adding bonus words stored in [b].*)
val empty_board : Yojson.Basic.t -> Yojson.Basic.t option -> int -> t

(** [requires_letters board word (row,col) dir] is the required letters
    to place [word] on [board] starting at [(row,col)] in direction
    horizontal if [dir] is true or vertical if [dir] is false. *)
val requires_letters : t -> string -> int * int -> bool -> char list

(** [place_word board word coord dir] is the new board after a legal
    [word] has been placed on the original [board] at position [coord]
    in direction horizontal if [dir] is true or vertical if [dir] is
    false.

    Raises: exception [IllegalMove] if the move is not legal. The move
    is legal if the move is near current board tiles, the move creates
    valid words, the move is on the board.*)
val place_word : t -> string -> int * int -> bool -> t

(** [get_created_words board word coord dir] is a list of all
    non-single-lettered words that will be created by the given move
    which places [word] on the original [board] at position [coord] in
    direction horizontal if [dir] is true or vertical if [dir] is false.

    Precondition: the move is legal. *)
val get_created_words :
  t -> string -> int * int -> bool -> (char * bonus) list list

(** [to_string board] is the string represntation of [board].*)
val to_string : t -> string

(** [print_board board] prints the board. The layout is similar to
    [to_string] but this display uses colors to display bonus tiles.*)
val print_board : t -> unit
