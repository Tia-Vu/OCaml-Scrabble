(** Representation of scrabble board data.

    This module represents the scrabble board data, including its shape
    (point distribution), current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(** Initializes an empty board with a dictionary stored with a certain
    json file.*)
val empty_board : Yojson.Basic.t -> t

(** [place_tiles] takes in a board and a move (word, starting
    coordinate, direction to place the word), and returns the result of
    the move

    Result is [Legal t] if the move is a valid move in scrabble (move is
    near current board tiles, move creates valid words, move is on the
    board) where t is the board after playing the move, otherwise result
    is [Illegal]*)

val place_tiles : t -> string -> int * int -> bool -> unit

(** String represntation of the board*)
val to_string : t -> string
