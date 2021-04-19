(** This module represents the current hand of tiles of a player in a
    scrabble game. *)

(** The abstract type of values representing a hand. *)
type t

(**A certain move can either be [Possible] or [Impossible] based on a
   certain hand.*)
type move_possibility =
  | Possible of t
  | Impossible

(**[make_move] takes in the original hand and the word to be taken from
   the hand, and is [Possible] with the new hand if the move is legal
   (the word can be made with tiles in the hand) and [Impossible] if the
   move is not legal (the word cannot be made with the tiles in the
   hand).*)
val make_move : t -> string -> move_possibility

(**new_hand is a new random hand taken from the tiles of a passed in
   tile pool.*)
val new_hand : Tilepool.t -> t

(**[to_list] is the hand but as a string list of each tile in the hand.*)
val to_list : t -> string list

(*Compare both, above is mine*)

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
