open OUnit2
open Board
open Game
open Display

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let board_tests = []

let game_tests = []

let suite =
  "test suite for Project" >::: List.flatten [ board_tests; game_tests ]

let _ = run_test_tt_main suite
