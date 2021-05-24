open OUnit2
open Board
open Hand
open Pool

(********************************************************************

  Test Plan

  The Game module and Display module is tested via playtest, as the
  interaction on the command line is the core experience of the modules.
  All other modules are tested by both unit tests and playtest.

  We mainly use blackbox testing as evaluating the actual edge cases in
  the gameplay is important - not causing errors is not enough. After we
  write such tests, we evaluate the coverage of the tests using bisect,
  and if the coverage is significantly low, we add more glassbox tesing
  so we don't get unexpected errors during the gameplay.

  The to_string function of each module is tested via playtest and not
  here since their correctness depends on their aesthetics, which the
  standard will easily and frequently change over time.

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

(** [cmp_unordered_lists lst1 lst2] compares two lists to see whether
    they are equivalent lists, irrelevant of their orders.*)
let cmp_unordered_lists compare lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  List.length lst1 = List.length sorted1
  && List.length lst2 = List.length sorted2
  && sorted1 = sorted2

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

let letter_lst_to_word (lst : char list) =
  let rec rec_ver lst acc =
    match lst with
    | h :: t -> rec_ver t (Char.escaped h ^ acc)
    | [] -> acc
  in
  rec_ver lst ""

let convert_to_word_list (lst : (char * Board.bonus) list list) =
  lst
  |> List.map (fun inner_lst -> List.map fst inner_lst)
  |> List.map letter_lst_to_word

(** [board_get_created_words_test name board word coord dir expected]
    constructs an OUnit test named [name] that asserts the quality of
    [Board.get_created_words], only the words, not the bonuses. *)
let board_get_created_words_test
    (name : string)
    (board : Board.t)
    (word : string)
    (coord : int * int)
    (dir : bool)
    (expected : string list) : test =
  "b_get_created_words_test: " ^ name >:: fun _ ->
  assert_equal expected
    (get_created_words board word coord dir |> convert_to_word_list)
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
  "h_spend_word_error: " ^ name >:: fun _ ->
  assert_raises InsufficentTiles (fun () ->
      Hand.spend_word letter_lst hand)

(** [score_update_test name words expected] constructs an OUnit test
    named [name] that asserts the score increment of
    [Score.update_score] with [expected]. *)
let score_update_test
    (name : string)
    (score : Score.t)
    (words : (char * Board.bonus) list list)
    (expected : int) : test =
  let rev_words = List.map List.rev words in
  let start_score = Score.get_score score in
  let updated_score =
    Score.get_score (Score.update_score score rev_words)
  in
  let increment = updated_score - start_score in
  "s_update: " ^ name >:: fun _ ->
  assert_equal expected increment ~printer:string_of_int

(********************************************************************
  End helper functions.
  ********************************************************************)

let dict = Yojson.Basic.from_file "dictionary.json"

let empty_board = Board.empty_board dict None 25

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
    board_get_created_words_test
      {|form a word from two words horizontal:
       place apple between the e and t of tree and not to make eat|}
      (place_word
         (place_word
            (place_word empty_board "run" (10, 11) false)
            "tree" (10, 10) true)
         "not" (12, 11) true)
      "apple" (11, 13) true [ "apple"; "eat" ];
    board_get_created_words_test
      {|form a word from two words vertical: 
      place apple between the e and t of tree and not to make eat|}
      (place_word
         (place_word
            (place_word empty_board "run" (11, 10) true)
            "tree" (10, 10) false)
         "not" (11, 12) false)
      "apple" (13, 11) false [ "apple"; "eat" ];
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

let vanila_score = Score.create None

let bonus_json = Yojson.Basic.from_string {|["camel","car"]|}

let wbonus_score = Score.create (Some bonus_json)

let score_tests =
  [
    score_update_test {|Empty word list|} vanila_score [ [] ] 0;
    score_update_test {|No bonus "apple"|} vanila_score
      [ [ ('a', N); ('p', N); ('p', N); ('l', N); ('e', N) ] ]
      9;
    score_update_test {|Double Letter on 'a', "apple"|} vanila_score
      [ [ ('a', DL); ('p', N); ('p', N); ('l', N); ('e', N) ] ]
      10;
    score_update_test {|Tripple Letter on 'a', "apple"|} vanila_score
      [ [ ('a', TL); ('p', N); ('p', N); ('l', N); ('e', N) ] ]
      11;
    score_update_test {|Double Letter on 'a','p', "apple"|} vanila_score
      [ [ ('a', TL); ('p', N); ('p', DL); ('l', N); ('e', N) ] ]
      14;
    score_update_test {|DL 'a', TL 'p', "apple"|} vanila_score
      [ [ ('a', DL); ('p', N); ('p', TL); ('l', N); ('e', N) ] ]
      16;
    score_update_test {|DW 'a', "apple"|} vanila_score
      [ [ ('a', DW); ('p', N); ('p', N); ('l', N); ('e', N) ] ]
      18;
    score_update_test {|DW 'l', "apple"|} vanila_score
      [ [ ('a', N); ('p', N); ('p', N); ('l', DW); ('e', N) ] ]
      18;
    score_update_test {|TW 'a', "apple"|} vanila_score
      [ [ ('a', TW); ('p', N); ('p', N); ('l', N); ('e', N) ] ]
      27;
    score_update_test {|TW 'l', "apple"|} vanila_score
      [ [ ('a', N); ('p', N); ('p', N); ('l', TW); ('e', N) ] ]
      27;
    score_update_test {|DW 'a', 'l', "apple"|} vanila_score
      [ [ ('a', DW); ('p', N); ('p', N); ('l', DW); ('e', N) ] ]
      36;
    score_update_test {|DW 'a', TW 'l', "apple"|} vanila_score
      [ [ ('a', DW); ('p', N); ('p', N); ('l', TW); ('e', N) ] ]
      54;
    score_update_test {|TW 'a', DW 'l', "apple"|} vanila_score
      [ [ ('a', TW); ('p', N); ('p', N); ('l', DW); ('e', N) ] ]
      54;
    score_update_test {|DL 'a', DW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', N); ('p', N); ('l', DW); ('e', N) ] ]
      20;
    score_update_test {|DL 'a','p', DW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', DL); ('p', N); ('l', DW); ('e', N) ] ]
      26;
    score_update_test {|DL 'a', TL 'p', DW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', TL); ('p', N); ('l', DW); ('e', N) ] ]
      32;
    score_update_test {|DL 'a', TW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', N); ('p', N); ('l', TW); ('e', N) ] ]
      30;
    score_update_test {|DL 'a','p', TW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', DL); ('p', N); ('l', TW); ('e', N) ] ]
      39;
    score_update_test {|DL 'a', TL 'p', TW 'l', "apple"|} vanila_score
      [ [ ('a', DL); ('p', TL); ('p', N); ('l', TW); ('e', N) ] ]
      48;
    score_update_test {|plain "car" and DW "cat" |} vanila_score
      [
        [ ('c', N); ('a', N); ('r', N) ];
        [ ('c', N); ('a', DW); ('t', N) ];
      ]
      15;
    score_update_test {|N "camel" (a bonus word)|} wbonus_score
      [ [ ('c', N); ('a', N); ('m', N); ('e', N); ('l', N) ] ]
      45;
    score_update_test {|DW "a" in "camel" (a bonus word)|} wbonus_score
      [ [ ('c', N); ('a', DW); ('m', N); ('e', N); ('l', N) ] ]
      90;
    score_update_test {|TW "a" in "camel" (a bonus word)|} wbonus_score
      [ [ ('c', N); ('a', TW); ('m', N); ('e', N); ('l', N) ] ]
      135;
    score_update_test {|DL "a" in "camel" (a bonus word)|} wbonus_score
      [ [ ('c', N); ('a', DL); ('m', N); ('e', N); ('l', N) ] ]
      50;
    score_update_test {|TL "a" in "camel" (a bonus word)|} wbonus_score
      [ [ ('c', N); ('a', TL); ('m', N); ('e', N); ('l', N) ] ]
      55;
    score_update_test {|DW 'c', DL "a"  in "camel" (a bonus word)|}
      wbonus_score
      [ [ ('c', DW); ('a', DL); ('m', N); ('e', N); ('l', N) ] ]
      100;
    score_update_test {|DW 'c', TL "a"  in "camel" (a bonus word)|}
      wbonus_score
      [ [ ('c', DW); ('a', TL); ('m', N); ('e', N); ('l', N) ] ]
      110;
    score_update_test {|TW 'c', DL "a"  in "camel" (a bonus word)|}
      wbonus_score
      [ [ ('c', TW); ('a', DL); ('m', N); ('e', N); ('l', N) ] ]
      150;
    score_update_test {|TW 'c', TL "a"  in "camel" (a bonus word)|}
      wbonus_score
      [ [ ('c', TW); ('a', TL); ('m', N); ('e', N); ('l', N) ] ]
      165;
    score_update_test {|plain bonus word "camel" and plain "cat" |}
      wbonus_score
      [
        [ ('c', N); ('a', N); ('m', N); ('e', N); ('l', N) ];
        [ ('c', N); ('a', N); ('t', N) ];
      ]
      50;
    score_update_test {|plain bonus word "camel" and DW "cat" |}
      wbonus_score
      [
        [ ('c', N); ('a', N); ('m', N); ('e', N); ('l', N) ];
        [ ('c', N); ('a', DW); ('t', N) ];
      ]
      55;
    score_update_test {|DW bonus word "camel" and plain "cat" |}
      wbonus_score
      [
        [ ('c', N); ('a', DW); ('m', N); ('e', N); ('l', N) ];
        [ ('c', N); ('a', N); ('t', N) ];
      ]
      95;
    score_update_test {|TW bonus word "camel" and  DL "a" in cat" |}
      wbonus_score
      [
        [ ('c', N); ('a', TW); ('m', N); ('e', N); ('l', N) ];
        [ ('c', N); ('a', DL); ('t', N) ];
      ]
      141;
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
           score_tests;
         ]

let _ = run_test_tt_main suite
