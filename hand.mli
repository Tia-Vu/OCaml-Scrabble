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
    possible. *)

val draw_nletters : Pool.t -> int -> t -> t

(** [has_word word hand] is [true] if and only if [hand] has the letters
    to make [word]*)

val has_word : string -> t -> bool

(** [spend_word word hand] gives new hand after spenidng letter tiles
    from [hand] to create [word].

    Requires: [has_word word hand = true] *)

val spend_word : string -> t -> t
