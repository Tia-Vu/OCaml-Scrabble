(** Representation of a player's hand in the game.*)
open Pool

(** The abstract type of values represnting a player's hand.*)
type t = char list

(** [empty_hand ()] gives an empty hand.*)
val empty_hand : unit -> t

(** [size hand] gives the size of [hand].*)
val size : t -> int

(** [draw_nletters pool n hand] gives a hand created after drawing [n]
    letters from [pool] to [hand]. If there are less than [n] letters in
    [pool], draw as much as possible.

    Requires: [n >= 0] *)
val draw_nletters : Pool.t -> int -> t -> t

(** [has_word word hand] is [true] if and only if [hand] has the letters
    to make [word].*)
val has_word : char list -> t -> bool

(** The exception [InsufficientTiles] is raised when there are not
    enough tiles in the hand to play a given move.*)
exception InsufficentTiles

(** [spend_word word hand] is the new hand after spending the
    appropriate letter tiles from [hand] to create [word].

    Raises: InsufficientTiles if [hand] does not have enough letter
    tiles to create [word]. *)
val spend_word : char list -> t -> t

(**[fill_hand pool max hand] is the hand created after drawing letters
   from [pool] to [hand] until it has [max] number of tiles.*)
val fill_hand : Pool.t -> int -> t -> t

(** [to_string hand] is the string representation of [hand].*)
val to_string : t -> string
