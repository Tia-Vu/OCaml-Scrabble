type t = char list

let empty_hand () = []

let size hand = List.length hand

let rec draw_nletters pool n hand =
  match n with
  | 0 -> hand
  | n ->
      if Pool.is_empty pool then hand
      else
        let letter = Pool.draw_letter pool in
        draw_nletters pool (n - 1) (letter :: hand)

(** to_letter_lst [word] returns [word] converted into a list of the
    letters in the list in the same order. Ex. to_letter_lst "hello"
    returns ['h';'e';'l';'l';'o']*)
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

(** [count_letter lc l] increments the count of [l] in [lc] by one, if
    [l] is a key in [lc]. Else, adds [(l,1)] to [lc]. *)
let add_letter_count lcount letter =
  match List.assoc_opt letter lcount with
  | Some count ->
      List.remove_assoc letter lcount |> List.cons (letter, count + 1)
  | None -> (letter, 1) :: lcount

let rec letter_count_rec acc = function
  | [] -> acc
  | letter :: t -> letter_count_rec (add_letter_count acc letter) t

(** [letter_count wlist] is an associative list of (letter,count)s which
    shows how many of each letter is in [word_list]. If a letter is not
    in the list, the letter is not in the word. *)
let letter_count word_list = letter_count_rec [] word_list

(** [has_enough_letters l c hlc] checks if [l] is in [hlc] and in its
    binded count in [hlc] is greater than or equal to [c] *)
let has_enough_letters letter count h_lcount =
  match List.assoc_opt letter h_lcount with
  | None -> false
  | Some h_count -> h_count >= count

(** [has_enough wlc hlc] checks if [hlc] has enough letters for each
    letter in [wlc] *)
let rec has_enough w_lcount h_lcount =
  match w_lcount with
  | [] -> true
  | (l, count) :: t ->
      if has_enough_letters l count h_lcount then has_enough t h_lcount
      else false

let has_word word hand =
  let w_lcount = letter_count (to_letter_lst word) in
  let h_lcount = letter_count hand in
  has_enough w_lcount h_lcount

(*PLACEHOLDER*)
let spend_word word hand = hand
