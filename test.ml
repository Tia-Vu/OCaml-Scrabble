open OUnit2
open Board
open Hand
open Pool

(********************************************************************
  Test Plan

  ********************************************************************)

(********************************************************************
   Helper functions for testing. 
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

let pp_board board =
  board |> Board.to_string |> fun x -> "\n-\n" ^ x ^ "\n-\n"

(* DEPRECATED: Board_to_string is tested through play test.
   [board_to_string name input expected_output] constructs an OUnit test
   named [name] that asserts the quality of [Board.to_str] with [index
   input]. *)
let board_to_string_test
    (name : string)
    (input : Board.t)
    (expected_output : string) : test =
  "board_to_str: " ^ name >:: fun _ ->
  assert_equal expected_output (Board.to_string input)
    ~printer:(fun x -> "\n-\n" ^ x ^ "\n-\n")

let board_place_word_test
    (name : string)
    (board : Board.t)
    (word : string)
    (coord : int * int)
    (dir : bool)
    (expected_output : Board.t) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (place_word board word coord dir)
    ~printer:pp_board

let board_illegal_place_word_test
    (name : string)
    (board : Board.t)
    (word : string)
    (coord : int * int)
    (dir : bool)
    (expected_error_msg : string) : test =
  "b_illegal_place_word_test: " ^ name >:: fun _ ->
  assert_raises (Board.IllegalMove expected_error_msg) (fun () ->
      place_word board word coord dir)

(** [draw_nletters_pool_size_test name n] constructs an OUnit test named
    [name] that asserts the quality of [Pool.draw_nletters] with the
    size of a new tile pool after drawing [n] letters.

    Requires: [init_pool] starts with 98 letters*)
let draw_nletters_pool_size_test (name : string) (n : int) : test =
  let pool = Pool.init_pool () in
  let _ = Hand.draw_nletters pool n (Hand.empty_hand ()) in
  "draw_nletters_pool_size_test: " ^ name >:: fun _ ->
  assert_equal (max 0 (98 - n)) (Pool.size pool) ~printer:string_of_int

let rec create_many_draw_nletters_psize_test test_list = function
  | -1 -> test_list
  | m ->
      create_many_draw_nletters_psize_test
        ( draw_nletters_pool_size_test ("n = " ^ string_of_int m) m
        :: test_list )
        (m - 1)

(** [draw_nletters_hand_size_test name n] constructs an OUnit test named
    [name] that asserts the quality of [Pool.draw_nletters] with the
    size of a new hand after drawing [n] letters.

    Requires: [init_pool] starts with 98 letters. *)
let draw_nletters_hand_size_test (name : string) (n : int) : test =
  let pool = Pool.init_pool () in
  let hand = Hand.draw_nletters pool n (Hand.empty_hand ()) in
  "draw_nletters_hand_size_test: " ^ name >:: fun _ ->
  assert_equal (min 98 n) (Hand.size hand) ~printer:string_of_int

let rec create_many_draw_nletters_hsize_test test_list = function
  | -1 -> test_list
  | m ->
      create_many_draw_nletters_hsize_test
        ( draw_nletters_hand_size_test ("n = " ^ string_of_int m) m
        :: test_list )
        (m - 1)

(** [has_word_test name word hand expected] constructs an OUnit test
    named [name] that asserts the quality of [Hand.has_word] with
    [expected]. *)
let has_word_test
    (name : string)
    (letter_lst : char list)
    (hand : Hand.t)
    (expected : bool) : test =
  "has_word: " ^ name >:: fun _ ->
  assert_equal expected
    (has_word letter_lst hand)
    ~printer:string_of_bool

(********************************************************************
  End helper functions.
  ********************************************************************)

let dict = Yojson.Basic.from_file "dictionary.json"

let empty_board = Board.empty_board dict 25

let board_tests =
  [
    board_illegal_place_word_test
      "Word places on negative row horizontally" empty_board "ant"
      (-1, 10) true "Word goes off board.";
    board_illegal_place_word_test
      "Word places on negative row vertically" empty_board "ant"
      (-1, 10) false "Word goes off board.";
    board_illegal_place_word_test
      "Word places on negative col horiontally" empty_board "ant"
      (10, -1) true "Word goes off board.";
    board_illegal_place_word_test
      "Word places on negative col vertically" empty_board "ant"
      (10, -1) false "Word goes off board.";
    board_illegal_place_word_test "Word places beyond row horizontally"
      empty_board "ant" (25, 10) true "Word goes off board.";
    board_illegal_place_word_test "Word places beyond row vertically"
      empty_board "ant" (25, 10) false "Word goes off board.";
    board_illegal_place_word_test "Word places beyond col horiontally"
      empty_board "ant" (10, 25) true "Word goes off board.";
    board_illegal_place_word_test "Word places beyond col vertically"
      empty_board "ant" (10, 25) false "Word goes off board.";
    board_illegal_place_word_test
      "Place horizontal word over vertical word."
      (place_word empty_board "ant" (10, 10) false)
      "sample" (10, 10) true "Tile tries to place on existing tiles.";
    board_illegal_place_word_test
      "Place vertical word over vertical word."
      (place_word empty_board "ant" (10, 10) false)
      "sample" (10, 10) false "Tile tries to place on existing tiles.";
    board_illegal_place_word_test
      "Place horizontal word over horizontal word."
      (place_word empty_board "ant" (10, 10) true)
      "sample" (10, 10) true "Tile tries to place on existing tiles.";
    board_illegal_place_word_test
      "Place vertical word over horizontal word."
      (place_word empty_board "ant" (10, 10) true)
      "sample" (10, 10) false "Tile tries to place on existing tiles.";
    board_illegal_place_word_test
      "Place horizontal word not near any existing tiles."
      (place_word empty_board "ant" (10, 10) true)
      "sample" (16, 16) true "Not near any existing tiles.";
    board_illegal_place_word_test
      "Place vertical word not near any existing tiles."
      (place_word empty_board "ant" (10, 10) true)
      "sample" (16, 16) false "Not near any existing tiles.";
    board_illegal_place_word_test "Start with a nonsensical word."
      empty_board "ecaml" (10, 10) true
      "Word \"ecaml\" is not in the dictionary.";
    board_illegal_place_word_test
      "Placeing a horizontal word on an adjacent row creates several \
       nonsensical words.CamlinternalLazy"
      (place_word empty_board "pineapple" (10, 10) true)
      "apple" (11, 10) true "Word \"ip\" is not in the dictionary.";
    board_illegal_place_word_test
      "Placeing a vertical word on an adjacent col creates several \
       nonsensical words."
      (place_word empty_board "pineapple" (10, 10) false)
      "apple" (10, 11) false "Word \"ip\" is not in the dictionary.";
    board_illegal_place_word_test
      "Extend a horizontal word into a nonsensical word."
      (place_word empty_board "pineapple" (10, 10) true)
      "cpineapple" (10, 9) true
      "Word \"cpineapple\" is not in the dictionary.";
    board_illegal_place_word_test
      "Extend a vertical word into a nonsensical word."
      (place_word empty_board "pineapple" (10, 10) false)
      "cpineapple" (9, 10) false
      "Word \"cpineapple\" is not in the dictionary.";
    (* Replaced by play tests board_to_string_test "Empty 1 x 1 board"
       (Board.empty_board dict 1) "."; board_to_string_test "Empty 2 x 2
       board" (Board.empty_board dict 2) ". .\n. .";
       board_to_string_test "Place 'c' horizontally on 4 x 4 board"
       (place_word (Board.empty_board dict 4) "c" (0, 0) true) "c . .
       .\n. . . .\n. . . .\n. . . ."; board_to_string_test "Place 'car'
       horizontally on 4 x 4 board" (place_word (Board.empty_board dict
       4) "car" (0, 0) true) "c a r .\n. . . .\n. . . .\n. . . .";
       board_to_string_test "Place 'car' vertically on 4 x 4 board"
       (place_word (Board.empty_board dict 4) "car" (0, 0) false) "c . .
       .\na . . .\nr . . .\n. . . ."; *)
  ]

let hand_tests =
  [
    has_word_test "apple in [a;p;p;l;e]"
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      true;
  ]

let draw_nletters_psize_test =
  create_many_draw_nletters_psize_test [] 100

let draw_nletters_hsize_test =
  create_many_draw_nletters_hsize_test [] 10

let game_tests = []

let suite =
  "test suite for Project"
  >::: List.flatten
         [
           board_tests;
           hand_tests;
           draw_nletters_psize_test;
           draw_nletters_hsize_test;
         ]

let _ = run_test_tt_main suite
