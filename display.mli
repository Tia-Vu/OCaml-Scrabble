open Board
open Score
open Hand
open Pool

val print_board : Board.t -> unit

val print_scores : Score.t -> unit

val print_hand : Hand.t -> unit

val print_round_start : unit -> unit

val print_round_end : unit -> unit

val print_separator : unit -> unit

val print_player_turn : int -> unit

val print_move_instructions : unit -> unit

val print_try_again : unit -> unit

val print_exc_board_illegal_move : string -> unit

val print_exc_malformed : unit -> unit

val print_exc_hand_insufficient_tiles : unit -> unit

val print_intro : unit -> unit

val print_end : unit -> unit

val print_pool : Pool.t -> unit
