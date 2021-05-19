(** Representation of tile pool of the current game.*)

(** The abstract type of values represnting tile pools.*)
type t

(** The exception [DrawFromEmpty] is raised when a letter is attempted
    to be drawn from an empty pool. *)
exception DrawFromEmpty

(** [init_pool ()] is a new tile pool.*)
val init_pool : unit -> t

(**[size p] is the size of the current tile_pool*)
val size : t -> int

(**[is_empty p] is true if and only if [p] is empty.*)
val is_empty : t -> bool

(* TODO: (to think about) Should we draw tiles instead of letters? *)

(**[draw_letter p] is a random letter from pool [p]

   Requires: [is_empty p] = false*)
val draw_letter : t -> char
