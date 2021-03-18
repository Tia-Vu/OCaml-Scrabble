(** Representation of scrabble board data.

    This module represents the scrabble board data, including
    its shape (point distribution), current letter tile placement. *)

(** The abstract type of values representing boards. *)
type t

(** TODO: Replace unit with something meaningful 
    Raises: TODO: something if input is illegal (word not in dict || tiles not near current tiles)*)
val place_tiles : t -> dictionary(?) -> tiles & their positions(?) -> t

(** String represntation of the board*)
val to_string : t -> string
