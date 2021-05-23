(** Representation of a player's score in the game.*)

(** The abstract type of values represnting a player's score.*)
type t

(** [update_score score words] is the new score with the appropriate
    amount of points added to a player's original [score] after they
    play [words] on the board.

    Requires: Each word in [words] is expected to be reverse in order.

    ex) for "camel" it should be
    [('l',N);('e',N);('m',N);('a',N);('c',N)] .*)
val update_score : t -> (char * Board.bonus) list list -> t

(**[create bonus] gives a new score with bonus words in [bonus].*)
val create : Yojson.Basic.t option -> t

(**[get_score score] gets the integer score value [score] is holding. *)
val get_score : t -> int

(**[to_string t] is the string representation of [t].*)
val to_string : t -> string
