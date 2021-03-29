open OUnit2
open Board

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

(** [board_to_string name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [Board.to_str] with
    [index input]. *)
let board_to_string_test
    (name : string)
    (input : Board.t)
    (expected_output : string) : test =
  "board_to_str: " ^ name >:: fun _ ->
  assert_equal expected_output (Board.to_string input)
    ~printer:(fun x -> "\n-\n" ^ x ^ "\n-\n")

(********************************************************************
  End helper functions.
  ********************************************************************)

let board_tests =
  [
    board_to_string_test "Empty 1 x 1 board" (Board.init_board 1) ".";
    board_to_string_test "Empty 2 x 2 board" (Board.init_board 2)
      ". .\n. .";
  ]

let game_tests = []

let suite =
  "test suite for Project" >::: List.flatten [ board_tests; game_tests ]

let _ = run_test_tt_main suite
