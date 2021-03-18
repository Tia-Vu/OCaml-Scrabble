(** Representation of scrabble board data.

    This module represents the scrabble board data, including
    its shape (point distribution), current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(** TODO: Replace unit with something meaningful 
    Requires: the input tiles should be legal *)
val place_tiles : t -> unit -> t

(** String represntation of the board*)
val to_string : t -> string
