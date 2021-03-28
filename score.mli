(** A score object contains the current points for each player in the
    game. *)

type t

(** [update_score] takes an old state of score and adds a new amount of
    points to the right player's score. TODO: how to pass in which
    player to add score to?*)
val update_score : t -> int -> t

val create : unit -> t
