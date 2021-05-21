open Board
open Yojson.Basic.Util

type t = {
  score : int;
  bonus_words : string list;
}

let bonus_words_from_json json =
  json |> Yojson.Basic.Util.to_list
  |> List.map (fun x -> Yojson.Basic.Util.to_string x)

let create json =
  { score = 0; bonus_words = bonus_words_from_json json }

let letter_score_dict =
  Yojson.Basic.from_file "letter_points.json"
  |> to_assoc
  |> List.map (fun (x, y) -> (x.[0], to_int y))

let letter_score lttr =
  match List.assoc_opt lttr letter_score_dict with
  | Some score -> score
  | None -> 0

(* DEPRECATED: match lttr with | 'a' -> 1 | 'b' -> 3 | 'c' -> 3 | 'd' ->
   2 | 'e' -> 1 | 'f' -> 4 | 'g' -> 2 | 'h' -> 4 | 'i' -> 1 | 'j' -> 8 |
   'k' -> 5 | 'l' -> 1 | 'm' -> 3 | 'n' -> 1 | 'o' -> 1 | 'p' -> 3 | 'q'
   -> 10 | 'r' -> 1 | 's' -> 1 | 't' -> 1 | 'u' -> 1 | 'v' -> 4 | 'w' ->
   4 | 'x' -> 8 | 'y' -> 4 | 'z' -> 10 | _ -> 0 *)

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
let is_bonus word t = List.mem word t.bonus_words

(*Note similar function in test.ml*)
let score_lst_to_word lst =
  let rec rec_ver lst acc =
    match lst with
    | (h, _) :: t -> rec_ver t (Char.escaped h ^ acc)
    | [] -> acc
  in
  rec_ver lst ""

let apply_bonus_words t letter_lst base_score =
  let word = score_lst_to_word letter_lst in
  if is_bonus word t then base_score * 5 else base_score

(*Gets the base score value of a certain word with letter bonuses
  applied*)
let base_word_score word =
  List.fold_left
    (fun acc (letter, bonus) ->
      acc + (letter_score letter |> apply_letter_bonus bonus))
    0 word

(*Gets the value that needs to be added to the score based on the words
  [words] that are formed by the move*)
let get_added_score t words =
  List.fold_left
    (fun acc word ->
      acc
      + ( base_word_score word |> apply_word_bonus word
        |> apply_bonus_words t word ))
    0 words

(*[update_score score new_words] returns the updated score given the new
  words [new_words] formed by a move*)
let update_score t new_words =
  {
    score = t.score + get_added_score t new_words;
    bonus_words = t.bonus_words;
  }

let get_score t = t.score

let to_string t = string_of_int t.score
