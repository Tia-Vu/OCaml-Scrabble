type t = char list

let empty_hand () = []

let size hand = List.length hand

exception InsufficentTiles

let rec draw_nletters pool n hand =
  match n with
  | 0 -> hand
  | n ->
      if Pool.is_empty pool then hand
      else
        let letter = Pool.draw_letter pool in
        draw_nletters pool (n - 1) (letter :: hand)

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

let has_word letter_lst hand =
  let w_lcount = letter_count letter_lst in
  let h_lcount = letter_count hand in
  has_enough w_lcount h_lcount

(** [subtract hlc wlc] reduces the count of letters in [hlc] by its
    corresponding letter's count in [wlc].*)
let rec subtract h_lcount w_lcount acc =
  match h_lcount with
  | [] -> acc
  | (c, count) :: t -> (
      match List.assoc_opt c w_lcount with
      | None -> subtract t w_lcount ((c, count) :: acc)
      | Some w_count -> subtract t w_lcount ((c, count - w_count) :: acc)
      )

(** [insert_nletters c n lst] inserts [c] to [lst] [n] times.

    Requires: [n] >= 0. *)
let rec insert_letter_ntimes letter n lst =
  match n with
  | 0 -> lst
  | n -> insert_letter_ntimes letter (n - 1) (letter :: lst)

(** [lcount_to_list acc lcount] creates a list of characters from
    [lcount].*)
let rec lcount_to_list acc = function
  | [] -> acc
  | (c, count) :: t ->
      lcount_to_list (insert_letter_ntimes c count acc) t

let spend_word letter_lst hand =
  let _ =
    if has_word letter_lst hand then () else raise InsufficentTiles
  in
  let w_lcount = letter_count letter_lst in
  let h_lcount = letter_count hand in
  let h_lcount' = subtract h_lcount w_lcount [] in
  lcount_to_list [] h_lcount'

let fill_hand pool max hand = draw_nletters pool (max - size hand) hand

let to_string hand =
  if hand = [] then ""
  else
    let s =
      List.fold_left (fun str c -> str ^ ", " ^ Char.escaped c) "" hand
    in
    String.sub s 1 (String.length s - 1)
