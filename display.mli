(** This module holds all utility functions related to printing
    information on the command prompt.*)

open Board
open Score
open Hand

(** [print_board board] pretty prints [board] on the command prompt.*)
val print_board : Board.t -> unit

(** [print_scores score] pretty prints [score] on the command prompt.*)
val print_scores : Score.t -> unit

(** [print_hand hand] pretty prints [hand] on the command prompt.*)
val print_hand : Hand.t -> unit

(** [print_round_start ()] declares the start of every round on the
    command prompt.*)
val print_round_start : unit -> unit

(** [print_round_end ()] declares the end of every round on the command
    prompt.*)
val print_round_end : unit -> unit

(** [print_separator ()] prints a line of dashes on the command prompt.*)
val print_separator : unit -> unit

(** [print_player_turn n] declares that it is player number [n]'s turn
    on the command prompt.*)
val print_player_turn : int -> unit

(** [print_move_instructions ()] prints instructions to tell players
    what format to input their move on the command prompt.*)
val print_move_instructions : unit -> unit

(** [print_try_again ()] tells users to try again on the command prompt.*)
val print_try_again : unit -> unit

(** [print_exc_board_illegal_move s] states that the move with word [s]
    was illegal on the command prompt.*)
val print_exc_board_illegal_move : string -> unit

(** [print_exc_malformed ()] states that the user input for a move was
    malformed on the command prompt.*)
val print_exc_malformed : unit -> unit

(** [print_exc_hand_insufficient_tiles ()] states a player's hand has
    insufficient tiles to play the move they entered on the command
    prompt.*)
val print_exc_hand_insufficient_tiles : unit -> unit

(** [print_intro ()] declares the start of the game on the command
    prompt.*)
val print_intro : unit -> unit

(** [print_end ()] declares the end of the game on the command prompt.*)
val print_end : unit -> unit
