(** Representation of tile pool of the current game*)

(** The abstract type of values represnting tile pools*)
type t

(** [init_pool ()] gives a new tile pool*)
val init_pool : unit -> t

(* TODO: (to think about) Should we draw tiles instead of letters? *)

(**[draw_letter p] draws a random letter from [p]*)
val draw_letter : t -> char
