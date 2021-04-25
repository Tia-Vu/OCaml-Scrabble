(** Representation of scrabble board data.

    This module represents the scrabble board data, including its shape
    (point distribution), current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(** Raised when an illegal move is attempted *)
exception IllegalMove of string

(** Initializes an empty board with a dictionary stored with a certain
    json file.*)
val empty_board : Yojson.Basic.t -> int -> t

(** [requires_letters board word (row,col) dir] returns the required
    letters to place [word] on [board] starting at [(row,col)] in
    direction [dir] *)
val requires_letters : t -> string -> int * int -> bool -> char list

(** [place_word] takes in a board and a move (word, starting coordinate,
    direction to place the word - [true] if horizontal, [false] if
    vertical), and returns the board after placing the word on board if
    the move is a valid move in scrabble (move is near current board
    tiles, move creates valid words, move is on the board) where t is
    the board after playing the move, otherwise raises [IllegalMove]*)

val place_word : t -> string -> int * int -> bool -> t

(** String represntation of the board*)
val to_string : t -> string
