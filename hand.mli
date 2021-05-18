(** Representation of a player's hand in the game*)
open Pool

(** The abstract type of values represnting hand*)
type t = char list

(** [empty_hand ()] creates an empty hand.*)

val empty_hand : unit -> t

(** [size hand] is the size of the hand.*)

val size : t -> int

(** [draw_nletters pool n hand] draws [n] letters from [pool] to hand.
    If there are less than [n] letters in [pool], draw as much as
    possible.

    Requires: n >= 0 *)

val draw_nletters : Pool.t -> int -> t -> t

(** [has_word word hand] is [true] if and only if [hand] has the letters
    to make [word]*)

val has_word : char list -> t -> bool

(** Raised when there are not enough tiles in the hand*)
exception InsufficentTiles

(** [spend_word word hand] gives new hand after spenidng letter tiles
    from [hand] to create [word].

    Raises: InsufficientTiles *)

val spend_word : char list -> t -> t

(**[fill_hand] draws letters from a pool and adds to the old hand until
   the max number of tiles in the hand is reached.*)
val fill_hand : Pool.t -> int -> t -> t

(** [to_string hand] converts [hand] to string.*)
val to_string : t -> string
