(** A score object contains the current points for each player in the
    game. *)

type t

(** [update_score score words] is the new score with the appropriate
    amount of points added to a player's original [score] after they
    play [words] on the board .*)
val update_score : t -> (char * Board.bonus) list list -> t

(**[create ()] is a new, initial score.*)
val create : unit -> t

(**[to_string t] is a string representation of [t]*)
val to_string : t -> string
