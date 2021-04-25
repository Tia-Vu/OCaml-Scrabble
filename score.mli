(** A score object contains the current points for each player in the
    game. *)

type t

(** [update_score] takes an old state of score and adds a new amount of
    points to the score based on which new words are formed*)
val update_score : t -> string list -> t

val create : unit -> t

val to_string : t -> string
