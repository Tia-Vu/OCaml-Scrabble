type t = int

let create () = 0

let to_letter_lst word =
  (*REMARK: The same function is in board*)
  let rec to_letter_lst_h word letter_lst =
    match word with
    | "" -> List.rev letter_lst
    | _ ->
        to_letter_lst_h
          (String.sub word 1 (String.length word - 1))
          (word.[0] :: letter_lst)
  in
  to_letter_lst_h word []

(*given the letter and bonus, it calculates the value of that tile
  placement on the board and updates the score*)
let add_bonus lttr_score bonus = failwith "unimplemented"

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

(*Gets the score value of a certain word*)
let word_score word =
  let letters = to_letter_lst word in
  List.fold_left (fun acc letter -> acc + letter_score letter) 0 letters

(*Gets the value that needs to be added based on the words [words] that
  are formed by the move*)
let get_added_score words =
  List.fold_left (fun acc word -> acc + word_score word) 0 words

(*[update_score score new_words] returns the updated score given the new
  words [new_words] formed by a move*)
let update_score scores new_words = scores + get_added_score new_words

let to_string score = string_of_int score
