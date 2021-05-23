(** This module holds all utility functions related to printing
    information on the command prompt.*)

open Board
open Score
open Hand
open Pool

(** [print_board board] pretty prints [board] on the command prompt.*)
val print_board : Board.t -> unit

(** [print_scores score] pretty prints [score] on the command prompt.*)
val print_scores : Score.t -> unit

(** [print_hand hand] pretty prints [hand] on the command prompt.*)
val print_hand : Hand.t -> unit

(** [print_round_start ()] prints the declaration of the start of every
    round on the command prompt.*)
val print_round_start : unit -> unit

(** [print_round_end ()] prints the declaration of the end of every
    round on the command prompt.*)
val print_round_end : unit -> unit

(** [print_separator ()] prints a line of dashes on the command prompt.*)
val print_separator : unit -> unit

(** [print_player_turn n] prints the delecaration of player number [n]'s
    turn on the command prompt.*)
val print_player_turn : int -> unit

(** [print_move_instructions ()] prints instructions to tell players the
    format to input their move on the command prompt.*)
val print_move_instructions : unit -> unit

(** [print_try_again ()] tells users to try again on the command prompt.*)
val print_try_again : unit -> unit

(** [print_exc_board_illegal_move s] prints that the move with word [s]
    was illegal on the command prompt.*)
val print_exc_board_illegal_move : string -> unit

(** [print_exc_malformed ()] prints that the user input for a move was
    malformed on the command prompt.*)
val print_exc_malformed : unit -> unit

(** [print_exc_hand_insufficient_tiles ()] prints that a player's hand
    has insufficient tiles to play the move they entered on the command
    prompt.*)
val print_exc_hand_insufficient_tiles : unit -> unit

(** [print_intro ()] prints the declaration of the start of the game on
    the command prompt.*)
val print_intro : unit -> unit

(** [print_winner score winners] prints the [winners] who won the game
    with highest score [score].*)
val print_winner : int -> int list -> unit

(** [print_end ()] prints the declaration of the end of the game on the
    command prompt.*)
val print_end : unit -> unit

(** [print_pool pool] prints the number of letters in the letter tiles
    on the command prompt.*)
val print_pool : Pool.t -> unit
