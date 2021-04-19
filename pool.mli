(** Representation of tile pool of the current game*)

(** The abstract type of values represnting tile pools*)
type t

(** Raised when the a letter is drawn from an empty pool. *)
exception DrawFromEmpty

(** [init_pool ()] gives a new tile pool*)
val init_pool : unit -> t

(**[size p] is the size of the current tile_pool*)
val size : t -> int

(**[is_empty p] is true if and only if [p] is empty.*)
val is_empty : t -> bool

(* TODO: (to think about) Should we draw tiles instead of letters? *)

(**[draw_letter p] draws a random letter from [p]

   Requires: [is_empty p] = false*)
val draw_letter : t -> char
