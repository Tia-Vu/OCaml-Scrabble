(** This module represents a pool of tiles for the current state of a
    Scrabble game. *)

(** The abstract type of values representing a tile pool. *)
type t

(**[new_pool] creates a brand new, random (?) pool of tiles. In the
   future: specify pool size?*)
val new_pool : unit -> t

(**[subtract_hand] takes in the original tile pool and a hand that can
   be generated from that pool, and subtracts the tiles in the hand from
   the original pool, returning the new smaller pool. Precondition: the
   hand must be generatable from the original pool.*)
val subtract_hand : t -> Hand.t -> t

(**[to_list] is the pool but as a string list of each tile in the pool.*)
val to_list : t -> string list
