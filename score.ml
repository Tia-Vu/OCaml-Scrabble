open Board

type t = int

let bonus_words =
  [
    "ocaml";
    "camel";
    "hump";
    "caravan";
    "desert";
    "oasis";
    "dromedary";
    "dromedary";
    "bactrian";
  ]

let create () = 0

let letter_score lttr =
  match lttr with
  | 'a' -> 1
  | 'b' -> 3
  | 'c' -> 3
  | 'd' -> 2
  | 'e' -> 1
  | 'f' -> 4
  | 'g' -> 2
  | 'h' -> 4
  | 'i' -> 1
  | 'j' -> 8
  | 'k' -> 5
  | 'l' -> 1
  | 'm' -> 3
  | 'n' -> 1
  | 'o' -> 1
  | 'p' -> 3
  | 'q' -> 10
  | 'r' -> 1
  | 's' -> 1
  | 't' -> 1
  | 'u' -> 1
  | 'v' -> 4
  | 'w' -> 4
  | 'x' -> 8
  | 'y' -> 4
  | 'z' -> 10
  | _ -> 0

let apply_letter_bonus bonus lttr_score =
  match bonus with
  | DL -> lttr_score * 2
  | TL -> lttr_score * 3
  | _ -> lttr_score

let word_bonus bonus word_score =
  match bonus with
  | DW -> word_score * 2
  | TW -> word_score * 3
  | _ -> word_score

let apply_word_bonus word base_score =
  List.fold_left
    (fun acc (letter, bonus) -> word_bonus bonus acc)
    base_score word

(*Based on a custom list of bonus words, returns whether the word is a
  bonus word*)
let is_bonus bonus word = List.mem word bonus

(*Gets the base score value of a certain word with letter bonuses
  applied*)
let base_word_score word =
  List.fold_left
    (fun acc (letter, bonus) ->
      acc + (letter_score letter |> apply_letter_bonus bonus))
    0 word

(*Gets the value that needs to be added to the score based on the words
  [words] that are formed by the move*)
let get_added_score words =
  List.fold_left
    (fun acc word ->
      acc + (base_word_score word |> apply_word_bonus word))
    0 words

(*[update_score score new_words] returns the updated score given the new
  words [new_words] formed by a move*)
let update_score score new_words = score + get_added_score new_words

let to_string score = string_of_int score
