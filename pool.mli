(** Representation of the tile pool of the current game.*)

(** The abstract type of values represnting tile pools.*)
type t

(** Exception [DrawFromEmpty] is raised when a letter is attempted to be
    drawn from an empty pool. *)
exception DrawFromEmpty

(** [init_pool ()] gives a new tile pool.*)
val init_pool : unit -> t

(**[size pool] gives the size of [pool].*)
val size : t -> int

(**[is_empty pool] is true if and only if [pool] is empty.*)
val is_empty : t -> bool

(**[draw_letter pool] draws a random letter from [pool].

   Requires: [is_empty pool = false].*)
val draw_letter : t -> char

(** [to_string pool] converts [pool] to a string, which shows the number
    of tiles in [pool].*)
val to_string : t -> string
