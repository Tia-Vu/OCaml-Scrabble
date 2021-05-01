open Board
open Score
open Hand

val print_board : Board.t -> unit

val print_scores : Score.t -> unit

val print_hand : Hand.t -> unit

val print_move_instructions : unit -> unit

val print_try_again : unit -> unit

val print_exc_board_illegal_move : string -> unit

val print_exc_malformed : unit -> unit

val print_exc_hand_insufficient_tiles : unit -> unit

val print_intro : unit -> unit

val print_end : unit -> unit
