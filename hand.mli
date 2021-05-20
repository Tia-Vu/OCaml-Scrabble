(** Representation of a player's hand in the game*)
open Pool

(** The abstract type of values represnting hand*)
type t = char list

(** [empty_hand ()] is an empty hand.*)

val empty_hand : unit -> t

(** [size hand] is the size of the hand.*)

val size : t -> int

(** [draw_nletters pool n hand] is a hand created from drawing [n]
    letters from [pool] to hand. If there are less than [n] letters in
    [pool], draw as much as possible.

    Requires: n >= 0 *)

val draw_nletters : Pool.t -> int -> t -> t

(** [has_word word hand] is [true] if and only if [hand] has the letters
    to make [word].*)

val has_word : char list -> t -> bool

(** The exception [InsufficientTiles] is raised when there are not
    enough tiles in the hand to play a given move.*)
exception InsufficentTiles

(** [spend_word word hand] is the new hand after spending the
    appropriate letter tiles from [hand] to create [word].

    Raises: InsufficientTiles *)

val spend_word : char list -> t -> t

(**[fill_hand pool max hand] is the hand created from drawing letters
   from [pool] and adds to the old [hand] until the [max] number of
   tiles in the hand is reached.*)
val fill_hand : Pool.t -> int -> t -> t

(** [to_string hand] is a string representation of a hand.*)

val to_string : t -> string
