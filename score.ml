type t = int

let create () = 0 (*PLACEHOLDER*)

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

(*PLACEHOLDER*)
let update_score scores score_to_add = scores + score_to_add

let to_string scores = string_of_int
