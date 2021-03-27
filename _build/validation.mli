(** This module signature is in charge of intaking a board state, an
    input move, * a dictionary, and deciding if that move is valid. *)

(** A move is either valid and awards some number of points or is
    invalid.*)
type validity =
  | Valid of int
  | Invalid

(**TODO: decide what data structures move and dict are.*)

(** [is_valid] is the validity of a move on a certain [board] and
    [dict]ionary.

    Typing here is bogus too: REPLACE the string lists with the types of
    moves and dicctionaries once the typing for those has been
    defined/implemented.*)
val is_valid : Board.t -> string list -> string list -> validity
