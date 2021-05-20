open OUnit2
open Board
open Hand
open Pool

(********************************************************************

  Test Plan

  We mainly use blackbox testing as evaluating the actual edge cases in
  the gameplay is important - not causing errors is not enough. After we
  write such tests, we evaluate the coverage of the tests using bisect,
  and if the coverage is significantly low, we add more glassbox tesing
  so we don't get unexpected errors during the gameplay. The to_string
  function of each module is tested via playtest and not here since
  their correctness depends on their aesthetics, which its standard will
  easily change over time, and the tests become outdated every time the
  standard changes.

  In this module there is a randomized testing for hand and pool module.
  pool has mutable structure, where the tiles in it are removed when
  [Hand.draw_nletters] is called. Thus in this module we draw various
  number of tiles (0~100 for pool and 0~10 for hand) and see if the size
  of the pool/hand reduced as much as we expected.

  ********************************************************************)

(********************************************************************
   Helper functions for testing. 
 ********************************************************************)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_char c] pretty-prints char [c]. *)
let pp_char c = "\"" ^ Char.escaped c ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst].

    Reference: Brought from CS3110 Spring Assignment a2*)
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

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order.

    Reference: Brought from CS3110 Spring Assignment a2*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [cmp_unordered_lists lst1 lst2] compares two lists to see whether
    they are equivalent lists, irrelevant of their orders.*)
let cmp_unordered_lists compare lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  List.length lst1 = List.length sorted1
  && List.length lst2 = List.length sorted2
  && sorted1 = sorted2

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

(** [board_illegal_place_word_test name board word coord dir
    expected_error_msg] constructs an OUnit test named [name] that
    asserts that [Board.place_word] raises [Board.IllegalMove] with
    string [expected_error_msg]. *)
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

(** [board_get_created_words_test name board word coord dir expected]
    constructs an OUnit test named [name] that asserts the quality of
    [Board.get_created_words]. *)
let board_get_created_words_test
    (name : string)
    (board : Board.t)
    (word : string)
    (coord : int * int)
    (dir : bool)
    (expected : string list) : test =
  "b_get_created_words_test: " ^ name >:: fun _ ->
  assert_equal expected
    (get_created_words board word coord dir)
    ~cmp:(cmp_unordered_lists String.compare)
    ~printer:(pp_list pp_string)

(** [board_requires_letters_test name board word coord dir expected]
    constructs an OUnit test named [name] that asserts the quality of
    [Board.requires_letters]. *)
let board_requires_letters_test
    (name : string)
    (board : Board.t)
    (word : string)
    (coord : int * int)
    (dir : bool)
    (expected : char list) : test =
  "b_requires_letters_test: " ^ name >:: fun _ ->
  assert_equal expected
    (Board.requires_letters board word coord dir)
    ~cmp:(cmp_unordered_lists Char.compare)
    ~printer:(pp_list pp_char)

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

(** [hand_has_word_test name word hand expected] constructs an OUnit
    test named [name] that asserts the quality of [Hand.has_word] with
    [expected]. *)
let hand_has_word_test
    (name : string)
    (letter_lst : char list)
    (hand : Hand.t)
    (expected : bool) : test =
  "h_has_word: " ^ name >:: fun _ ->
  assert_equal expected
    (Hand.has_word letter_lst hand)
    ~printer:string_of_bool

(** [hand_spend_word_test name word hand expected] constructs an OUnit
    test named [name] that asserts the quality of [Hand.spend_word] with
    [expected]. *)
let hand_spend_word_test
    (name : string)
    (letter_lst : char list)
    (hand : Hand.t)
    (expected : Hand.t) : test =
  "h_spend_word: " ^ name >:: fun _ ->
  assert_equal expected
    (Hand.spend_word letter_lst hand)
    ~printer:(pp_list pp_char)
    ~cmp:(cmp_unordered_lists Char.compare)

(** [hand_spend_word_error_test name word hand] constructs an OUnit test
    named [name] that asserts that [Hand.spend_word] raises
    [InsufficientTiles]. *)
let hand_spend_word_error_test
    (name : string)
    (letter_lst : char list)
    (hand : Hand.t) : test =
  "h_spend_word_error_test: " ^ name >:: fun _ ->
  assert_raises InsufficentTiles (fun () ->
      Hand.spend_word letter_lst hand)

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
    board_get_created_words_test
      "Place a horizontal word on empty board" empty_board "apple"
      (10, 10) true [ "apple" ];
    board_get_created_words_test "Place a vertical word on empty board"
      empty_board "apple" (10, 10) false [ "apple" ];
    board_get_created_words_test
      {|horizontal "ba" below horizontal "apple"|}
      (place_word empty_board "apple" (10, 10) true)
      "ba" (11, 10) true [ "ab"; "ba"; "pa" ];
    board_get_created_words_test
      {|vertical "ba" on the right of vertical "apple"|}
      (place_word empty_board "apple" (10, 10) false)
      "ba" (10, 11) false [ "ab"; "ba"; "pa" ];
    board_get_created_words_test
      {|extend horizontal word (input only the new prefix)|}
      (place_word empty_board "apple" (10, 10) true)
      "pine" (10, 6) true [ "pineapple" ];
    board_get_created_words_test
      {|extend horizontal word (input whole word)|}
      (place_word empty_board "apple" (10, 10) true)
      "pineapple" (10, 6) true [ "pineapple" ];
    board_get_created_words_test
      {|extend vertical word (input only the new prefix)|}
      (place_word empty_board "apple" (10, 10) false)
      "pine" (6, 10) false [ "pineapple" ];
    board_get_created_words_test
      {|extend vertical word (input whole word)|}
      (place_word empty_board "apple" (10, 10) false)
      "pineapple" (6, 10) false [ "pineapple" ];
    board_get_created_words_test
      {|extend horizontal word (input only the new prefix)|}
      (place_word empty_board "do" (10, 10) true)
      "a" (10, 9) true [ "ado" ];
    board_get_created_words_test
      {|extend horizontal word (input only the new suffix)|}
      (place_word empty_board "pine" (10, 10) true)
      "apple" (10, 14) true [ "pineapple" ];
    board_requires_letters_test "Place a horizontal word on empty board"
      empty_board "apple" (10, 10) true
      [ 'a'; 'p'; 'p'; 'l'; 'e' ];
    board_requires_letters_test "Place a vertical word on empty board"
      empty_board "apple" (10, 10) false
      [ 'a'; 'p'; 'p'; 'l'; 'e' ];
    board_requires_letters_test
      {|horizontal "ba" below horizontal "apple"|}
      (place_word empty_board "apple" (10, 10) true)
      "ba" (11, 10) true [ 'b'; 'a' ];
    board_requires_letters_test
      {|vertical "ba" on the right of vertical "apple"|}
      (place_word empty_board "apple" (10, 10) false)
      "ba" (10, 11) false [ 'b'; 'a' ];
    board_requires_letters_test
      {|extend horizontal word (input only the new prefix)|}
      (place_word empty_board "apple" (10, 10) true)
      "pine" (10, 6) true [ 'p'; 'i'; 'n'; 'e' ];
    board_requires_letters_test
      {|extend horizontal word (input whole word)|}
      (place_word empty_board "apple" (10, 10) true)
      "pineapple" (10, 6) true [ 'p'; 'i'; 'n'; 'e' ];
    board_requires_letters_test
      {|extend vertical word (input only the new prefix)|}
      (place_word empty_board "apple" (10, 10) false)
      "pine" (6, 10) false [ 'p'; 'i'; 'n'; 'e' ];
    board_requires_letters_test
      {|extend vertical word (input whole word)|}
      (place_word empty_board "apple" (10, 10) false)
      "pineapple" (6, 10) false [ 'p'; 'i'; 'n'; 'e' ];
    board_requires_letters_test
      {|extend horizontal word (input only the new prefix)|}
      (place_word empty_board "do" (10, 10) true)
      "a" (10, 9) true [ 'a' ];
    board_requires_letters_test
      {|extend horizontal word (input only the new suffix)|}
      (place_word empty_board "pine" (10, 10) true)
      "apple" (10, 14) true
      [ 'a'; 'p'; 'p'; 'l'; 'e' ];
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
    hand_has_word_test "apple in [a;p;p;l;e]"
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      true;
    hand_has_word_test "apple in [p;p;l;e;d;a]"
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'p'; 'p'; 'l'; 'e'; 'd'; 'a' ]
      true;
    hand_has_word_test "apple not in []"
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [] false;
    hand_has_word_test "apple not in [a;p;l;e]"
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'a'; 'p'; 'l'; 'e' ] false;
    hand_spend_word_test {|spend "cape" from [c;c;a;a;p;p;e;e]|}
      [ 'c'; 'a'; 'p'; 'e' ]
      [ 'c'; 'c'; 'a'; 'a'; 'p'; 'p'; 'e'; 'e' ]
      [ 'c'; 'a'; 'p'; 'e' ];
    hand_spend_word_test {|spend "apple" from [e;a;p;p;l]|}
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'e'; 'a'; 'p'; 'p'; 'l' ]
      [];
    hand_spend_word_error_test {|spend "apple" from empty hand|}
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      (Hand.empty_hand ());
    hand_spend_word_error_test {|spend "apple" from [a;p;l;e|}
      [ 'a'; 'p'; 'p'; 'l'; 'e' ]
      [ 'a'; 'p'; 'l'; 'e' ];
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
