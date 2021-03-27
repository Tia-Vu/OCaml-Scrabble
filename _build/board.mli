(** Representation of scrabble board data.

    This module represents the scrabble board data, including its shape
    (point distribution), current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(** Initializes an empty board with a dictionary stored with a certain
    json file.*)
val empty_board : Yojson.Basic.t -> t

(** [place_tiles] takes in a board and a move, and returns a pair of the
    new board * and the point value that the move was worth. TODO:
    Replace unit with something meaningful. Raises: TODO: something if
    input is illegal (word not in dict || tiles not near current tiles)

    This method typing is kind of bogus atm, REPLACE later when
    implementation is in.*)

val place_tiles : t -> string -> t * int

(** String represntation of the board*)
val to_string : t -> string
